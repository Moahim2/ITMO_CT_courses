#include "requests.h"

#include "string.h"
namespace {

void encode_new_order_opt_fields(unsigned char * bitfield_start,
                                 const double price,
                                 const char ord_type,
                                 const char time_in_force,
                                 const unsigned max_floor,
                                 const std::string & symbol,
                                 const char capacity,
                                 const std::string & account)
{
    auto * p = bitfield_start + new_order_bitfield_num(RequestType::New);
#define FIELD(name, bitfield_num, bit)                    \
    set_opt_field_bit(bitfield_start, bitfield_num, bit); \
    p = encode_field_##name(p, name);
#include "new_order_opt_fields.inl"
}

unsigned char * order_set_opt_field_bits(unsigned char * bitfield_start, const RequestType type)
{
    *bitfield_start++ = new_order_bitfield_num(type);
    auto * p = bitfield_start + new_order_bitfield_num(type);

    switch (type) {
    case RequestType::New:
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "new_order_opt_fields.inl"
        break;
    case RequestType::NewCross:
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "new_order_cross_opt_fields.inl"
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "new_order_cross_special_opt_fields.inl"
        break;
    case RequestType::NewCrossMultileg:
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "new_order_cross_multilegs_opt_fields.inl"
#define FIELD(name, bitfield_num, bit) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit);
#include "new_order_cross_special_opt_fields.inl"
    }
    return p;
}

uint8_t encode_request_type(const RequestType type)
{
    switch (type) {
    case RequestType::New:
        return 0x38;
    case RequestType::NewCross:
        return 0x7A;
    case RequestType::NewCrossMultileg:
        return 0x85;
    }
}

unsigned char * add_request_header(unsigned char * start, unsigned length, const RequestType type, unsigned seq_no)
{
    *start++ = 0xBA;
    *start++ = 0xBA;
    start = encode(start, static_cast<uint16_t>(length));
    start = encode(start, encode_request_type(type));
    *start++ = 0;
    return encode(start, static_cast<uint32_t>(seq_no));
}

char convert_side(const Side side)
{
    switch (side) {
    case Side::Buy: return '1';
    case Side::Sell: return '2';
    }
    return 0;
}

char convert_ord_type(const OrdType ord_type)
{
    switch (ord_type) {
    case OrdType::Market: return '1';
    case OrdType::Limit: return '2';
    case OrdType::Pegged: return 'P';
    }
    return 0;
}

char convert_time_in_force(const TimeInForce time_in_force)
{
    switch (time_in_force) {
    case TimeInForce::Day: return '0';
    case TimeInForce::IOC: return '3';
    case TimeInForce::GTD: return '6';
    }
    return 0;
}

char convert_capacity(const Capacity capacity)
{
    switch (capacity) {
    case Capacity::Agency: return 'A';
    case Capacity::Principal: return 'P';
    case Capacity::RisklessPrincipal: return 'R';
    }
    return 0;
}

char convert_account_type(const AccountType account_type)
{
    switch (account_type) {
    case AccountType::Client: return '1';
    case AccountType::House: return '3';
    }
    return '0';
}

char convert_position(const Position position)
{
    switch (position) {
    case Position::Open: return 'O';
    case Position::Close: return 'C';
    case Position::None: return 'N';
    }
    return 0;
}

std::string convert_legs(const std::vector<Position> & legs)
{
    std::string str;
    str.reserve(legs.size());
    for (const auto p : legs) {
        str += convert_position(p);
    }
    return str;
}

unsigned char * encode_new_order_cross_bitfields(unsigned char * bitfield_start, const bool algorithmic_indicator)
{
#define FIELD(name, _, __) \
    bitfield_start = encode_field_##name(bitfield_start, name);
#include "new_order_cross_opt_fields.inl"
    return bitfield_start;
}

unsigned char * encode_new_order_cross_multilegs_bitfields(unsigned char * bitfield_start,
                                                           const std::string & leg_position_effects,
                                                           const bool algorithmic_indicator)
{ // leg_position_effects здесь передается вроде по той же логике, что и std::string & symbol в изначальном коде морозова (такое же опциональное поле)
#define FIELD(name, _, __) \
    bitfield_start = encode_field_##name(bitfield_start, name);
#include "new_order_cross_multilegs_opt_fields.inl"
    return bitfield_start;
}

unsigned char * encode_new_order_cross_special_bitfields(unsigned char * bitfield_start, const std::string & symbol)
{
#define FIELD(name, _, __) \
    bitfield_start = encode_field_##name(bitfield_start, name);
#include "new_order_cross_special_opt_fields.inl"
    return bitfield_start;
}

unsigned char * encode_gen_part_dif_order(unsigned char * start, const Order & order)
{
    start = encode_char(start, convert_side(order.side));
    start = encode_binary4(start, static_cast<uint32_t>(order.volume));
    start = encode_text(start, order.cl_ord_id, 20);
    start = encode_char(start, convert_capacity(order.capacity));
    start = encode_text(start, order.clearing_firm, 4);
    return encode_char(start, convert_account_type(order.account_type));
}

unsigned char * encode_order(unsigned char * start, const Order & order)
{
    start = encode_gen_part_dif_order(start, order);
    return encode_new_order_cross_bitfields(start, order.algorithmic_indicator);
}

unsigned char * encode_complex_order(unsigned char * start, const ComplexOrder & complOrder)
{
    start = encode_gen_part_dif_order(start, complOrder.order);
    return encode_new_order_cross_multilegs_bitfields(start, convert_legs(complOrder.legs), complOrder.order.algorithmic_indicator);
}

unsigned char * encode_orders(unsigned char * start, const Order & agency_order, const std::vector<Order> & contra_orders)
{
    start = encode_order(start, agency_order);
    for (size_t i = 0; i < contra_orders.size(); i++) {
        start = encode_order(start, contra_orders[i]);
    }
    return start;
}

unsigned char * encode_complex_orders(unsigned char * start, const ComplexOrder & agency_order, const std::vector<ComplexOrder> & contra_orders)
{
    start = encode_complex_order(start, agency_order);
    for (size_t i = 0; i < contra_orders.size(); i++) {
        start = encode_complex_order(start, contra_orders[i]);
    }
    return start;
}
} // anonymous namespace

std::array<unsigned char, calculate_size(RequestType::New)> create_new_order_request(const unsigned seq_no,
                                                                                     const std::string & cl_ord_id,
                                                                                     const Side side,
                                                                                     const double volume,
                                                                                     const double price,
                                                                                     const OrdType ord_type,
                                                                                     const TimeInForce time_in_force,
                                                                                     const double max_floor,
                                                                                     const std::string & symbol,
                                                                                     const Capacity capacity,
                                                                                     const std::string & account)
{
    static_assert(calculate_size(RequestType::New) == 78, "Wrong New Order message size");

    std::array<unsigned char, calculate_size(RequestType::New)> msg;
    auto * p = add_request_header(&msg[0], msg.size() - 2, RequestType::New, seq_no);
    p = encode_text(p, cl_ord_id, 20);
    p = encode_char(p, convert_side(side));
    p = encode_binary4(p, static_cast<uint32_t>(volume));
    p = encode(p, static_cast<uint8_t>(new_order_bitfield_num(RequestType::New)));
    encode_new_order_opt_fields(p,
                                price,
                                convert_ord_type(ord_type),
                                convert_time_in_force(time_in_force),
                                max_floor,
                                symbol,
                                convert_capacity(capacity),
                                account);
    return msg;
}

std::vector<unsigned char> create_new_order_cross_request(
        unsigned seq_no,
        const std::string & cross_id,
        double price,
        const std::string & symbol,
        const Order & agency_order,
        const std::vector<Order> & contra_orders)
{
    const size_t size_new_order_cross = 47 + new_order_bitfield_num(RequestType::NewCross) +
            (contra_orders.size() + 1) * calculate_size(RequestType::NewCross);

    std::vector<unsigned char> msg(size_new_order_cross);
    auto * p = add_request_header(&msg[0], msg.size() - 2, RequestType::NewCross, seq_no);
    p = encode_text(p, cross_id, 20);
    *p++ = '1';
    p = encode_char(p, convert_side(agency_order.side));
    p = encode_price(p, price);
    p = encode_binary4(p, static_cast<uint32_t>(agency_order.volume));
    p = order_set_opt_field_bits(p, RequestType::NewCross);
    p = encode(p, static_cast<uint16_t>(contra_orders.size() + 1));
    p = encode_orders(p, agency_order, contra_orders);
    encode_new_order_cross_special_bitfields(p, symbol);
    return msg;
}

std::vector<unsigned char> create_new_order_cross_multileg_request(
        unsigned seq_no,
        const std::string & cross_id,
        double price,
        const std::string & symbol,
        const ComplexOrder & agency_order,
        const std::vector<ComplexOrder> & contra_orders)
{
    const size_t size_new_order_cross_multileg = 47 + new_order_bitfield_num(RequestType::NewCrossMultileg) +
            (contra_orders.size() + 1) * calculate_size(RequestType::NewCrossMultileg);

    std::vector<unsigned char> msg(size_new_order_cross_multileg);
    auto * p = add_request_header(&msg[0], msg.size() - 2, RequestType::NewCrossMultileg, seq_no);
    p = encode_text(p, cross_id, 20);
    *p++ = '1';
    p = encode_char(p, convert_side(agency_order.order.side));
    p = encode_price(p, price);
    p = encode_binary4(p, static_cast<uint32_t>(agency_order.order.volume));
    p = order_set_opt_field_bits(p, RequestType::NewCrossMultileg);
    p = encode(p, static_cast<uint16_t>(contra_orders.size() + 1));
    p = encode_complex_orders(p, agency_order, contra_orders);
    encode_new_order_cross_special_bitfields(p, symbol);
    return msg;
}
