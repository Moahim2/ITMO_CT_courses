# BOE кодеры

## Кодирование сообщений NewOrderCross и NewOrderCrossMultiLeg
### Идея
Некоторые участники электронных торгов осуществляют сделки как от своего собственного лица, так и от лица своих клиентов, не имеющих
прямого доступа на биржу. Возможна ситуация, когда такой брокер получает от клиента заказ, который он может выполнить сам, из своей
собственной ликвидности (например, клиент хочет купить N акций компании XXX, брокер имеет достаточный запас этих акций и готов их
продать клиенту). Однако, во многих юрисдикциях брокеры не имеют права устанавливать произвольные цены в рамках таких операций, более
того, обязаны предоставить своим клиентам "лучшее исполнение", то есть обеспечить получение лучшей возможной в этот момент времени цены.
Обязательства эти действуют в определённых рамках, зачастую схема выглядит так:
- брокер получает заказ клиента и выставляет свою цену
- брокер посылает два заказа на рынок - клиентский и свой, зеркальный
- рынок объявляет мини-аукцион - в течении определённого короткого времени другие участники могут предложить свои заказы, полностью
или частично выполняющие заказ клиента брокера
- по окончании такого аукциона заказ клиента полностью исполнен, но для заказа брокера это не гарантировано

Разные биржи реализуют такой функционал по-разному, но обычно они предоставляют участникам функцию посылки зеркальных заказов, как одного
целого, атомарно. Это называется "cross order".

На площадках Cboe C1 и CEDX поддерживается отправка таких "кросс" заказов с возможностью для брокера выставить несколько встречных заказов,
возможно, из разных источников. В совокупности они должны полностью покрывать заказ клиента, всего может быть 10 таких "кусочков".
Таким образом, всего в рыночной транзакции будет от 2 до 11 заказов, один из которых - клиентский, а остальные имеют противоположную сторону.

Дополнительным нюансом являются т.н. комбинации или сложные инструменты, которые представляют объединение нескольких других инструментов
в одно целое, торгуемое, как обычный инструмент. В данном задании мы не будет углубляться в эту тему и лишь зададим число "ног" для сложных
инструментов (т.е. число вложенных инструментов).

### Задание
Требуется реализовать кодеры для сообщений NewOrderCross и NewOrderCrossMultileg (см. [protocol specifications](doc/Cboe_Europe_CEDX_BOE2_Specification-2.16.pdf)),
имеющие следующий интерфейс:
```cpp
enum class AccountType {
    Client,
    House
};

struct Order
{
    Side side;
    double volume;
    std::string cl_ord_id;
    Capacity capacity;
    std::string clearing_firm;
    AccountType account_type;
    bool algorithmic_indicator;
};

std::vector<unsigned char> create_new_order_cross_request(
  unsigned seq_no,
  const std::string & cross_id,
  double price,
  const std::string & symbol,
  const Order & agency_order,
  const std::vector<Order> & contra_orders);

enum class Position {
    Open,
    Close
};

struct ComplexOrder
{
    Order order;
    std::vector<Position> legs;
};

std::vector<unsigned char> create_new_order_cross_multileg_request(
  unsigned seq_no,
  const std::string & cross_id,
  double price,
  const std::string & symbol,
  const ComplexOrder & agency_order,
  const std::vector<ComplexOrder> & contra_orders);
```

Поле `CrossPrioritization` требуется заполнить исходя из стороны `agency_order` (значение `CrossPrioritization` = значение `agency_order.side`).

Поле `OrderQty` требуется заполнить из значения `agency_order.volume`.

Гарантируется, что для всех элементов `contra_orders` значение поле `side` совпадает.

Значения `volume` элементов `contra_orders` суммируются к `agency_order.volume`, проверять это не требуется.

Прочие поля:
* seq_no -> SequenceNumber
* cross_id -> CrossId
* price -> Price
* symbol -> Symbol
* order.side -> Side
* order.volume -> AllocQty
* order.capacity -> Capacity
* order.clearing_firm -> ClearingFirm
* order.account_type -> AccountType
* order.algorithmic_indicator -> AlgorithmicIndicator
* complex_order.legs -> LegPositionEffects

Работа с `std::string`:
```cpp
std::string str;
...
std::size_t len = str.size(); // длина
char c = str[i]; // i - неотрицательный целочисленный индекс, нумерается начинается с 0
..
// обход всех символов строки
for (std::size_t i = 0; i < str.size(); ++i) {
    // do something with str[i]
}
```

Работа с `std::vector`:
```cpp
std::vector<Order> orders;
...
// обход элементов
for (std::size_t i = 0; i < orders.size(); ++i) {
    double alloc_qty = orders[i].volume;
    ...
}

// наполнение массива байт известной длины
std::vector<unsigned char> data;
data.resize(size);
unsigned char * ptr = &data[0];
```
