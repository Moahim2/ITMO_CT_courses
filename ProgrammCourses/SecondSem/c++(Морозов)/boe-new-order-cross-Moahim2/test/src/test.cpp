#include "requests.h"

#include <gtest/gtest.h>

#include <cstring>
#include <ostream>
#include <type_traits>

namespace {

template <std::size_t Size>
class String
{
    char data_[Size + 1];
public:
    String(const unsigned char * src)
    {
        std::memcpy(data_, src, Size);
        data_[Size] = '\0';
    }
    String(const String & other)
    {
        std::memcpy(data_, other.data_, Size + 1);
    }
    String(const std::string & str)
    {
        std::memset(data_, 0, Size + 1);
        std::memcpy(data_, str.c_str(), str.size());
    }
    template <std::size_t Length>
    String(const char (& str)[Length])
    {
        std::memset(data_, 0, Size + 1);
        std::memcpy(data_, str, Length);
    }

    friend bool operator == (const String & lhs, const String & rhs)
    { return 0 == std::memcmp(lhs.data_, rhs.data_, Size + 1); }
    friend bool operator != (const String & lhs, const String & rhs)
    { return !(lhs == rhs); }
    friend bool operator == (const String & lhs, const char * rhs)
    { return lhs == std::string(rhs); }
    friend bool operator != (const String & lhs, const char * rhs)
    { return !(lhs == rhs); }
    friend bool operator == (const char * lhs, const String & rhs)
    { return std::string(lhs) == rhs; }
    friend bool operator != (const char * lhs, const String & rhs)
    { return !(lhs == rhs); }
    friend bool operator == (const std::string & lhs, const String & rhs)
    { return lhs == rhs.data_; }
    friend bool operator != (const std::string & lhs, const String & rhs)
    { return !(lhs == rhs); }
    friend bool operator == (const String & lhs, const std::string & rhs)
    { return rhs == lhs; }
    friend bool operator != (const String & lhs, const std::string & rhs)
    { return rhs != lhs; }
    friend std::ostream & operator << (std::ostream & strm, const String & s)
    { return strm << s.data_; }
};

template <class T>
inline constexpr bool is_string_v = false;
template <std::size_t Size>
inline constexpr bool is_string_v<String<Size>> = true;

template <class T>
inline constexpr std::size_t string_size_v = 0;
template <std::size_t Size>
inline constexpr std::size_t string_size_v<String<Size>> = Size;

struct integral_type {};
struct string_type {};

template <class T>
struct any_to_false : std::false_type {};

template <class T>
T read_value_impl(const unsigned char * data, const std::size_t offset, const integral_type)
{
    const T * x = reinterpret_cast<const T *>(data + offset);
    return *x;
}

template <std::size_t Size>
String<Size> read_value_impl(const unsigned char * data, const std::size_t offset, const string_type)
{
    return {data + offset};
}

template <class T>
T read_value_impl(const unsigned char * data, const std::size_t offset)
{
    if constexpr (std::is_integral_v<T>) {
        return read_value_impl<T>(data, offset, integral_type{});
    }
    else if constexpr (is_string_v<T>) {
        return read_value_impl<string_size_v<T>>(data, offset, string_type{});
    }
    else {
        static_assert(any_to_false<T>::value, "Non-implemented type");
        return {};
    }
}

template <>
double read_value_impl<double>(const unsigned char * data, const std::size_t offset)
{
    const int64_t * x = reinterpret_cast<const int64_t *>(data + offset);
    return static_cast<double>(*x) / 10000;
}

template <class T, class A>
T read_value(const A & data, const std::size_t offset)
{
    return read_value_impl<T>(data.data(), offset);
}

struct NewOrderData
{
    static char convert_side(const Side side)
    {
        switch (side) {
            case Side::Buy: return '1';
            case Side::Sell: return '2';
        }
        return 0;
    }
    static char convert_ord_type(const OrdType ord_type)
    {
        switch (ord_type) {
            case OrdType::Market: return '1';
            case OrdType::Limit: return '2';
            case OrdType::Pegged: return 'P';
        }
        return 0;
    }
    static char convert_time_in_force(const TimeInForce time_in_force)
    {
        switch (time_in_force) {
            case TimeInForce::Day: return '0';
            case TimeInForce::IOC: return '3';
            case TimeInForce::GTD: return '6';
        }
        return 0;
    }
    static char convert_capacity(const Capacity capacity)
    {
        switch (capacity) {
            case Capacity::Agency: return 'A';
            case Capacity::Principal: return 'P';
            case Capacity::RisklessPrincipal: return 'R';
        }
        return 0;
    }

    const unsigned seq_no;
    const std::string cl_ord_id;
    const char side;
    const double volume;
    const double price;
    const char ord_type;
    const char time_in_force;
    const double max_floor;
    const std::string symbol;
    const char capacity;
    const std::string account;
    const std::array<unsigned char, calculate_size(RequestType::New)> data;

    NewOrderData(
              const unsigned seq_no_
            , std::string cl_ord_id_
            , const Side side_
            , const double volume_
            , const double price_
            , const OrdType ord_type_
            , const TimeInForce time_in_force_
            , const double max_floor_
            , std::string symbol_
            , const Capacity capacity_
            , std::string account_
            )
        : seq_no(seq_no_)
        , cl_ord_id(std::move(cl_ord_id_))
        , side(convert_side(side_))
        , volume(volume_)
        , price(price_)
        , ord_type(convert_ord_type(ord_type_))
        , time_in_force(convert_time_in_force(time_in_force_))
        , max_floor(max_floor_)
        , symbol(std::move(symbol_))
        , capacity(convert_capacity(capacity_))
        , account(std::move(account_))
        , data(create_new_order_request(seq_no, cl_ord_id, side_, volume, price, ord_type_, time_in_force_, max_floor, symbol, capacity_, account))
    {}
};

class TestRequest
{
    unsigned seq_no_ = 1;
    std::string cl_ord_id_{"ORD101"};
    Side side_ = Side::Buy;
    double volume_ = 100;
    double price_ = 12.505;
    OrdType ord_type_ = OrdType::Limit;
    TimeInForce time_in_force_ = TimeInForce::Day;
    double max_floor_ = 10;
    std::string symbol_{"AAPl"};
    Capacity capacity_ = Capacity::Principal;
    std::string account_{"ACC331"};
public:
#define FIELD(field, type) \
    TestRequest && field(type field ## _v) && \
    { \
        field ## _ = std::move(field ## _v); \
        return std::move(*this); \
    }
    FIELD(seq_no, unsigned)
    FIELD(cl_ord_id, std::string)
    FIELD(side, Side)
    FIELD(volume, double)
    FIELD(price, double)
    FIELD(ord_type, OrdType)
    FIELD(time_in_force, TimeInForce)
    FIELD(max_floor, double)
    FIELD(symbol, std::string)
    FIELD(capacity, Capacity)
    FIELD(account, std::string)
#undef FIELD
    operator NewOrderData () &&
    {
        return {
            seq_no_
          , std::move(cl_ord_id_)
          , side_
          , volume_
          , price_
          , ord_type_
          , time_in_force_
          , max_floor_
          , std::move(symbol_)
          , capacity_
          , std::move(account_)
        };
    }
};

template <class From, class To>
struct FieldData
{
    From from;
    To to;

    FieldData(const From & f, const To & t)
        : from(f)
        , to(t)
    { }
};

template <std::size_t LegNum>
struct CrossMultilegData
{
    FieldData<Side, char> side;
    FieldData<double, uint32_t> volume;
    FieldData<std::string, String<20>> cl_ord_id;
    FieldData<Capacity, char> capacity;
    FieldData<std::string, String<4>> clearing_firm;
    FieldData<AccountType, char> account_type;
    FieldData<bool, char> algorithmic_indicator;
    FieldData<std::array<Position, LegNum>, String<12>> legs;
};

} // anonymous namespace

TEST(NewOrderTest, simple_size_check)
{
    const NewOrderData nod = TestRequest();
    EXPECT_EQ(78, nod.data.size());
}

TEST(NewOrderTest, read_values)
{
    const NewOrderData nod = TestRequest()
        .volume(33)
        .time_in_force(TimeInForce::IOC)
        .capacity(Capacity::Agency)
        .account("HOUSE1");
    const auto header = read_value<uint16_t>(nod.data, 0);
    const auto msg_len = read_value<uint16_t>(nod.data, 2);
    const auto msg_type = read_value<uint8_t>(nod.data, 4);
    const auto seq_no = read_value<uint32_t>(nod.data, 6);
    const auto cl_ord_id = read_value<String<20>>(nod.data, 10);
    const auto side = read_value<char>(nod.data, 30);
    const auto volume = read_value<uint32_t>(nod.data, 31);
    const auto price = read_value<double>(nod.data, 39);
    const auto ord_type = read_value<char>(nod.data, 47);
    const auto time_in_force = read_value<char>(nod.data, 48);
    const auto max_floor = read_value<uint32_t>(nod.data, 49);
    const auto symbol = read_value<String<8>>(nod.data, 53);
    const auto capacity = read_value<char>(nod.data, 61);
    const auto account = read_value<String<16>>(nod.data, 62);
    EXPECT_EQ(0xBABA, header);
    EXPECT_EQ(nod.data.size()-2, msg_len);
    EXPECT_EQ(0x38, msg_type);
    EXPECT_EQ(nod.seq_no, seq_no);
    EXPECT_EQ(nod.cl_ord_id, cl_ord_id);
    EXPECT_EQ(nod.side, side);
    EXPECT_EQ(nod.volume, volume);
    EXPECT_EQ(nod.price, price);
    EXPECT_EQ(nod.ord_type, ord_type);
    EXPECT_EQ(nod.time_in_force, time_in_force);
    EXPECT_EQ(nod.max_floor, max_floor);
    EXPECT_EQ(nod.symbol, symbol);
    EXPECT_EQ(nod.capacity, capacity);
    EXPECT_EQ(nod.account, account);
}

TEST(NewOrderCrossTest, two_orders)
{
    const auto msg = create_new_order_cross_request(
            13,
            "CROSS19",
            1.01,
            "00Q0kA",
            {Side::Buy, 999, "A123456x", Capacity::Agency, "CLR1", AccountType::Client, false},
            { {Side::Sell, 999, "T98765u", Capacity::Principal, "CLR1", AccountType::House, false} });

    const auto msg_id = read_value<uint8_t>(msg, 4);
    const auto seq_num = read_value<uint32_t>(msg, 6);
    const auto cross_id = read_value<String<20>>(msg, 10);
    const auto cross_type = read_value<char>(msg, 30);
    const auto cross_prio = read_value<char>(msg, 31);
    const auto price = read_value<double>(msg, 32);
    const auto volume = read_value<uint32_t>(msg, 40);
    const auto bitfield_num = read_value<uint8_t>(msg, 44);
    const auto bitfield1 = read_value<uint8_t>(msg, 45);
    const auto group_cnt = read_value<uint16_t>(msg, 46);
    const auto client_side = read_value<char>(msg, 48);
    const auto client_volume = read_value<uint32_t>(msg, 49);
    const auto client_cl_ord_id = read_value<String<20>>(msg, 53);
    const auto client_capacity = read_value<char>(msg, 73);
    const auto client_clearing_firm = read_value<String<4>>(msg, 74);
    const auto client_account_type = read_value<char>(msg, 78);
    const auto client_algo = read_value<char>(msg, 79);
    const auto house_side = read_value<char>(msg, 80);
    const auto house_volume = read_value<uint32_t>(msg, 81);
    const auto house_cl_ord_id = read_value<String<20>>(msg, 85);
    const auto house_capacity = read_value<char>(msg, 105);
    const auto house_clearing_firm = read_value<String<4>>(msg, 106);
    const auto house_account_type = read_value<char>(msg, 110);
    const auto house_algo = read_value<char>(msg, 111);
    const auto symbol = read_value<String<8>>(msg, 112);
    EXPECT_EQ(0x7A, msg_id);
    EXPECT_EQ(13, seq_num);
    EXPECT_EQ("CROSS19", cross_id);
    EXPECT_EQ('1', cross_type);
    EXPECT_EQ('1', cross_prio);
    EXPECT_DOUBLE_EQ(1.01, price);
    EXPECT_EQ(999, volume);
    EXPECT_EQ(1, bitfield_num);
    EXPECT_EQ(65, bitfield1);
    EXPECT_EQ(2, group_cnt);
    EXPECT_EQ('1', client_side);
    EXPECT_EQ(999, client_volume);
    EXPECT_EQ("A123456x", client_cl_ord_id);
    EXPECT_EQ('A', client_capacity);
    EXPECT_EQ("CLR1", client_clearing_firm);
    EXPECT_EQ('1', client_account_type);
    EXPECT_EQ('N', client_algo);
    EXPECT_EQ('2', house_side);
    EXPECT_EQ(999, house_volume);
    EXPECT_EQ("T98765u", house_cl_ord_id);
    EXPECT_EQ('P', house_capacity);
    EXPECT_EQ("CLR1", house_clearing_firm);
    EXPECT_EQ('3', house_account_type);
    EXPECT_EQ('N', house_algo);
    EXPECT_EQ("00Q0kA", symbol);
}

TEST(NewOrderCrossTest, eleven_orders)
{
    const auto msg = create_new_order_cross_request(
            32769,
            "NZ1V7BJ1AcceptBuy",
            85899345.199999,
            "00Q0kA",
            {Side::Sell, 10000, "A123456x", Capacity::Agency, "CLR1", AccountType::Client, false},
            {
                {Side::Buy, 1000, "T98765u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 1500, "T98766u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 500, "T98767u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 1000, "T98768u", Capacity::Agency, "CLR1", AccountType::Client, true}
              , {Side::Buy, 1000, "T98769u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 1000, "T98760u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 1000, "T98761u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 1000, "T98762u", Capacity::Principal, "CLR1", AccountType::House, true}
              , {Side::Buy, 1990, "T98763u", Capacity::Principal, "CLR1", AccountType::House, false}
              , {Side::Buy, 10, "T98764u", Capacity::RisklessPrincipal, "CLR1", AccountType::House, true}
            });

    const auto msg_id = read_value<uint8_t>(msg, 4);
    const auto seq_num = read_value<uint32_t>(msg, 6);
    const auto cross_id = read_value<String<20>>(msg, 10);
    const auto cross_type = read_value<char>(msg, 30);
    const auto cross_prio = read_value<char>(msg, 31);
    const auto price = read_value<double>(msg, 32);
    const auto volume = read_value<uint32_t>(msg, 40);
    const auto bitfield_num = read_value<uint8_t>(msg, 44);
    const auto bitfield1 = read_value<uint8_t>(msg, 45);
    const auto group_cnt = read_value<uint16_t>(msg, 46);
    const auto client_side = read_value<char>(msg, 48);
    const auto client_volume = read_value<uint32_t>(msg, 49);
    const auto client_cl_ord_id = read_value<String<20>>(msg, 53);
    const auto client_capacity = read_value<char>(msg, 73);
    const auto client_clearing_firm = read_value<String<4>>(msg, 74);
    const auto client_account_type = read_value<char>(msg, 78);
    const auto client_algo = read_value<char>(msg, 79);
    std::size_t offs = 80;
    std::vector<std::tuple<char, uint32_t, String<20>, char, String<4>, char, char>> house_orders;
    for (std::size_t i = 0; i < 10; ++i) {
        house_orders.emplace_back(
                read_value<char>(msg, offs),
                read_value<uint32_t>(msg, offs+1),
                read_value<String<20>>(msg, offs+5),
                read_value<char>(msg, offs+25),
                read_value<String<4>>(msg, offs+26),
                read_value<char>(msg, offs+30),
                read_value<char>(msg, offs+31));
        offs += 32;
    }
    const auto symbol = read_value<String<8>>(msg, offs);
    EXPECT_EQ(0x7A, msg_id);
    EXPECT_EQ(32769, seq_num);
    EXPECT_EQ("NZ1V7BJ1AcceptBuy", cross_id);
    EXPECT_EQ('1', cross_type);
    EXPECT_EQ('2', cross_prio);
    EXPECT_NEAR(85899345.1999, price, 0.00001);
    EXPECT_EQ(10000, volume);
    EXPECT_EQ(1, bitfield_num);
    EXPECT_EQ(65, bitfield1);
    EXPECT_EQ(11, group_cnt);
    EXPECT_EQ('2', client_side);
    EXPECT_EQ(10000, client_volume);
    EXPECT_EQ("A123456x", client_cl_ord_id);
    EXPECT_EQ('A', client_capacity);
    EXPECT_EQ("CLR1", client_clearing_firm);
    EXPECT_EQ('1', client_account_type);
    EXPECT_EQ('N', client_algo);
    EXPECT_EQ('1', std::get<0>(house_orders[0])) << "Unexpected Side for a first order";
    EXPECT_EQ(1000, std::get<1>(house_orders[0])) << "Unexpected AllocQty for a first order";
    EXPECT_EQ("T98765u", std::get<2>(house_orders[0])) << "Unexpected ClOrdId for a first order";
    EXPECT_EQ('P', std::get<3>(house_orders[0])) << "Unexpected Capacity for a first order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[0])) << "Unexpected ClearingFirm for a first order";
    EXPECT_EQ('3', std::get<5>(house_orders[0])) << "Unexpected AccountType for a first order";
    EXPECT_EQ('N', std::get<6>(house_orders[0])) << "Unexpected AlgorithmIndicator for a first order";
    EXPECT_EQ('1', std::get<0>(house_orders[1])) << "Unexpected Side for a second order";
    EXPECT_EQ(1500, std::get<1>(house_orders[1])) << "Unexpected AllocQty for a second order";
    EXPECT_EQ("T98766u", std::get<2>(house_orders[1])) << "Unexpected ClOrdId for a second order";
    EXPECT_EQ('P', std::get<3>(house_orders[1])) << "Unexpected Capacity for a second order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[1])) << "Unexpected ClearingFirm for a second order";
    EXPECT_EQ('3', std::get<5>(house_orders[1])) << "Unexpected AccountType for a second order";
    EXPECT_EQ('N', std::get<6>(house_orders[1])) << "Unexpected AlgorithmIndicator for a second order";
    EXPECT_EQ('1', std::get<0>(house_orders[2])) << "Unexpected Side for a third order";
    EXPECT_EQ(500, std::get<1>(house_orders[2])) << "Unexpected AllocQty for a third order";
    EXPECT_EQ("T98767u", std::get<2>(house_orders[2])) << "Unexpected ClOrdId for a third order";
    EXPECT_EQ('P', std::get<3>(house_orders[2])) << "Unexpected Capacity for a third order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[2])) << "Unexpected ClearingFirm for a third order";
    EXPECT_EQ('3', std::get<5>(house_orders[2])) << "Unexpected AccountType for a third order";
    EXPECT_EQ('N', std::get<6>(house_orders[2])) << "Unexpected AlgorithmIndicator for a third order";
    EXPECT_EQ('1', std::get<0>(house_orders[3])) << "Unexpected Side for a fourth order";
    EXPECT_EQ(1000, std::get<1>(house_orders[3])) << "Unexpected AllocQty for a fourth order";
    EXPECT_EQ("T98768u", std::get<2>(house_orders[3])) << "Unexpected ClOrdId for a fourth order";
    EXPECT_EQ('A', std::get<3>(house_orders[3])) << "Unexpected Capacity for a fourth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[3])) << "Unexpected ClearingFirm for a fourth order";
    EXPECT_EQ('1', std::get<5>(house_orders[3])) << "Unexpected AccountType for a fourth order";
    EXPECT_EQ('Y', std::get<6>(house_orders[3])) << "Unexpected AlgorithmIndicator for a fourth order";
    EXPECT_EQ('1', std::get<0>(house_orders[4])) << "Unexpected Side for a fifth order";
    EXPECT_EQ(1000, std::get<1>(house_orders[4])) << "Unexpected AllocQty for a fifth order";
    EXPECT_EQ("T98769u", std::get<2>(house_orders[4])) << "Unexpected ClOrdId for a fifth order";
    EXPECT_EQ('P', std::get<3>(house_orders[4])) << "Unexpected Capacity for a fifth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[4])) << "Unexpected ClearingFirm for a fifth order";
    EXPECT_EQ('3', std::get<5>(house_orders[4])) << "Unexpected AccountType for a fifth order";
    EXPECT_EQ('N', std::get<6>(house_orders[4])) << "Unexpected AlgorithmIndicator for a fifth order";
    EXPECT_EQ('1', std::get<0>(house_orders[5])) << "Unexpected Side for a sixth order";
    EXPECT_EQ(1000, std::get<1>(house_orders[5])) << "Unexpected AllocQty for a sixth order";
    EXPECT_EQ("T98760u", std::get<2>(house_orders[5])) << "Unexpected ClOrdId for a sixth order";
    EXPECT_EQ('P', std::get<3>(house_orders[5])) << "Unexpected Capacity for a sixth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[5])) << "Unexpected ClearingFirm for a sixth order";
    EXPECT_EQ('3', std::get<5>(house_orders[5])) << "Unexpected AccountType for a sixth order";
    EXPECT_EQ('N', std::get<6>(house_orders[5])) << "Unexpected AlgorithmIndicator for a sixth order";
    EXPECT_EQ('1', std::get<0>(house_orders[6])) << "Unexpected Side for a seventh order";
    EXPECT_EQ(1000, std::get<1>(house_orders[6])) << "Unexpected AllocQty for a seventh order";
    EXPECT_EQ("T98761u", std::get<2>(house_orders[6])) << "Unexpected ClOrdId for a seventh order";
    EXPECT_EQ('P', std::get<3>(house_orders[6])) << "Unexpected Capacity for a seventh order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[6])) << "Unexpected ClearingFirm for a seventh order";
    EXPECT_EQ('3', std::get<5>(house_orders[6])) << "Unexpected AccountType for a seventh order";
    EXPECT_EQ('N', std::get<6>(house_orders[6])) << "Unexpected AlgorithmIndicator for a seventh order";
    EXPECT_EQ('1', std::get<0>(house_orders[7])) << "Unexpected Side for a eighth order";
    EXPECT_EQ(1000, std::get<1>(house_orders[7])) << "Unexpected AllocQty for a eighth order";
    EXPECT_EQ("T98762u", std::get<2>(house_orders[7])) << "Unexpected ClOrdId for a eighth order";
    EXPECT_EQ('P', std::get<3>(house_orders[7])) << "Unexpected Capacity for a eighth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[7])) << "Unexpected ClearingFirm for a eighth order";
    EXPECT_EQ('3', std::get<5>(house_orders[7])) << "Unexpected AccountType for a eighth order";
    EXPECT_EQ('Y', std::get<6>(house_orders[7])) << "Unexpected AlgorithmIndicator for a eighth order";
    EXPECT_EQ('1', std::get<0>(house_orders[8])) << "Unexpected Side for a ninth order";
    EXPECT_EQ(1990, std::get<1>(house_orders[8])) << "Unexpected AllocQty for a ninth order";
    EXPECT_EQ("T98763u", std::get<2>(house_orders[8])) << "Unexpected ClOrdId for a ninth order";
    EXPECT_EQ('P', std::get<3>(house_orders[8])) << "Unexpected Capacity for a ninth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[8])) << "Unexpected ClearingFirm for a ninth order";
    EXPECT_EQ('3', std::get<5>(house_orders[8])) << "Unexpected AccountType for a ninth order";
    EXPECT_EQ('N', std::get<6>(house_orders[8])) << "Unexpected AlgorithmIndicator for a ninth order";
    EXPECT_EQ('1', std::get<0>(house_orders[9])) << "Unexpected Side for a tenth order";
    EXPECT_EQ(10, std::get<1>(house_orders[9])) << "Unexpected AllocQty for a tenth order";
    EXPECT_EQ("T98764u", std::get<2>(house_orders[9])) << "Unexpected ClOrdId for a tenth order";
    EXPECT_EQ('R', std::get<3>(house_orders[9])) << "Unexpected Capacity for a tenth order";
    EXPECT_EQ("CLR1", std::get<4>(house_orders[9])) << "Unexpected ClearingFirm for a tenth order";
    EXPECT_EQ('3', std::get<5>(house_orders[9])) << "Unexpected AccountType for a tenth order";
    EXPECT_EQ('Y', std::get<6>(house_orders[9])) << "Unexpected AlgorithmIndicator for a tenth order";
    EXPECT_EQ("00Q0kA", symbol);
}

TEST(NewOrderCrossMultilegTest, two_orders)
{
    CrossMultilegData<4> client_order{
        {Side::Sell, '2'},
        {100, 100},
        {"E12345u", "E12345u"},
        {Capacity::Agency, 'A'},
        {"CLR3", "CLR3"},
        {AccountType::Client, '1'},
        {false, 'N'},
        {{Position::Open, Position::Open, Position::Close, Position::Close}, "OOCC"}
    };
    CrossMultilegData<4> house_order{
        {Side::Buy, '1'},
        {100, 100},
        {"Wyxz111", "Wyxz111"},
        {Capacity::Principal, 'P'},
        {"CLR3", "CLR3"},
        {AccountType::House, '3'},
        {false, 'N'},
        {{Position::Close, Position::Open, Position::Close, Position::Open}, "COCO"}
    };
    const auto msg = create_new_order_cross_multileg_request(
            61111,
            "CrossCombo",
            12.34,
            "00Q0kA",
            {{client_order.side.from, client_order.volume.from, client_order.cl_ord_id.from, client_order.capacity.from, client_order.clearing_firm.from, client_order.account_type.from, client_order.algorithmic_indicator.from}, {client_order.legs.from.begin(), client_order.legs.from.end()}},
            { {{house_order.side.from, house_order.volume.from, house_order.cl_ord_id.from, house_order.capacity.from, house_order.clearing_firm.from, house_order.account_type.from, house_order.algorithmic_indicator.from}, {house_order.legs.from.begin(), house_order.legs.from.end()}} });

    const auto msg_id = read_value<uint8_t>(msg, 4);
    const auto seq_num = read_value<uint32_t>(msg, 6);
    const auto cross_id = read_value<String<20>>(msg, 10);
    const auto cross_type = read_value<char>(msg, 30);
    const auto cross_prio = read_value<char>(msg, 31);
    const auto price = read_value<double>(msg, 32);
    const auto volume = read_value<uint32_t>(msg, 40);
    const auto bitfield_num = read_value<uint8_t>(msg, 44);
    const auto bitfield1 = read_value<uint8_t>(msg, 45);
    const auto bitfield2 = read_value<uint8_t>(msg, 46);
    const auto bitfield3 = read_value<uint8_t>(msg, 47);
    const auto group_cnt = read_value<uint16_t>(msg, 48);
    const auto client_side = read_value<char>(msg, 50);
    const auto client_volume = read_value<uint32_t>(msg, 51);
    const auto client_cl_ord_id = read_value<String<20>>(msg, 55);
    const auto client_capacity = read_value<char>(msg, 75);
    const auto client_clearing_firm = read_value<String<4>>(msg, 76);
    const auto client_account_type = read_value<char>(msg, 80);
    const auto client_legs = read_value<String<12>>(msg, 81);
    const auto client_algo = read_value<char>(msg, 93);
    const auto house_side = read_value<char>(msg, 94);
    const auto house_volume = read_value<uint32_t>(msg, 95);
    const auto house_cl_ord_id = read_value<String<20>>(msg, 99);
    const auto house_capacity = read_value<char>(msg, 119);
    const auto house_clearing_firm = read_value<String<4>>(msg, 120);
    const auto house_account_type = read_value<char>(msg, 124);
    const auto house_legs = read_value<String<12>>(msg, 125);
    const auto house_algo = read_value<char>(msg, 137);
    const auto symbol = read_value<String<8>>(msg, 138);
    EXPECT_EQ(0x85, msg_id);
    EXPECT_EQ(61111, seq_num);
    EXPECT_EQ("CrossCombo", cross_id);
    EXPECT_EQ('1', cross_type);
    EXPECT_EQ('2', cross_prio);
    EXPECT_DOUBLE_EQ(12.34, price);
    EXPECT_EQ(100, volume);
    EXPECT_EQ(3, bitfield_num);
    EXPECT_EQ(65, bitfield1);
    EXPECT_EQ(0, bitfield2);
    EXPECT_EQ(16, bitfield3);
    EXPECT_EQ(2, group_cnt);
    EXPECT_EQ(client_order.side.to, client_side);
    EXPECT_EQ(client_order.volume.to, client_volume);
    EXPECT_EQ(client_order.cl_ord_id.to, client_cl_ord_id);
    EXPECT_EQ(client_order.capacity.to, client_capacity);
    EXPECT_EQ(client_order.clearing_firm.to, client_clearing_firm);
    EXPECT_EQ(client_order.account_type.to, client_account_type);
    EXPECT_EQ(client_order.algorithmic_indicator.to, client_algo);
    EXPECT_EQ(client_order.legs.to, client_legs);
    EXPECT_EQ(house_order.side.to, house_side);
    EXPECT_EQ(house_order.volume.to, house_volume);
    EXPECT_EQ(house_order.cl_ord_id.to, house_cl_ord_id);
    EXPECT_EQ(house_order.capacity.to, house_capacity);
    EXPECT_EQ(house_order.clearing_firm.to, house_clearing_firm);
    EXPECT_EQ(house_order.account_type.to, house_account_type);
    EXPECT_EQ(house_order.algorithmic_indicator.to, house_algo);
    EXPECT_EQ(house_order.legs.to, house_legs);
    EXPECT_EQ("00Q0kA", symbol);
}
