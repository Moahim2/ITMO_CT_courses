# Сложные типы данных

## Неквалифицированный и квалифицированный поиск.
Если нет `::`, то идентификатор неквалифицированный.  
**Пример 1:**
```C++
#include <iostream>
int main() {
  struct std{};
  std::cout << "fail\n"; // Ошибка: неквалифицированный поиск для 'std' находит структуру
  ::std::cout << "ok\n"; // ОК: ::std находит пространство имен std (полностью квалифицированное)
}
```
**Пример 2:**
```C++
std::string::npos // является выражением, называющим статический член в npos в string
                  // классе в пространстве имен std
```
**Пример 3:**
```C++
::tolower // является выражением, называющим функцию tolower в глобальном пространстве имен
```
**Пример 4:**
```C++
int side;
int main() 
{
  Side side;
  std::cout << ::side; // обратились к глобальной переменной
}
// Если слева от :: ничего не стоит, это "корень"
```

## Перечисления

### Строгие перечисления
```C++
enum class Side {Buy, Sell};
void print(const Side side) {
  switch (side) {
    case Side::Buy:
      std::cout << "Buy";
      break;
    case Side::Sell:
      std::cout << "Sell";
      break;
  }
}
```
Чтобы обратиться к константам из строгого перечисления всегда нужно указывать полное имя: 
```C++ 
Enum_name::Element_name
```
Если хотим по элементу `enum` получить его интегральное значение, то нужно сделать явное приведение этого элемента к нужному типу:  
```C++
static_cast<int>(Side::Sell);
```  
По умолчанию `enum` хранится в `int`.  
Первая константа получает значение `0`.
Далее, для каждой следующей константы, если явного значения не задано, значение по умолчанию инкрементируется на `1`.
```C++
enum class Side{
    Buy = 100,
    Sell = 15
};

enum class Side : unsigned long { // можем так сказать, что числа в enum будут unsigned long
    Buy,
    Sell
};
```

### Нестрогие перечисления
```C++
enum Side {Buy, Sell};
```
Их еще называют `unscoped enum`.  
В отличие от строгих перечислений, у нестрогих имена констант попадают в окружующую область видимости (пространство имен или класс), поэтому не нужно писать полное имя.

Еще отличие в том, что работает неявное преобразование из значение `enum` в `int`.  
Иногда это может плохо для нас закончиться:  

```C++
enum Side {Buy, Sell};
enum Capacity {Agency, Principal, RisklessPrincipal};

void process(const Side side, const Capacity capacity) {
  if (side == Agency) {
    std::cout << "Ha-ha!\n";
  }
}

int main() {
  process(Buy, Principal);
}
```
Лучше их не использовать.


## Структуры

```C++
struct Rational {
  int numerator;
  int denominator;
} a; // так мы сделали объект структуры
     // он проинициализирован нулями, т.к это глобальная переменная

int main() {
  std::cout << a.numerator << "/" << a.denominator << "\n"; // 0 0
}
```

Если же сделать так:
```C++
struct Rational {
  int numerator;
  int denominator;
};

int main() {
  Rational a; // инициализация мусором
  std::cout << a.numerator << "/" << a.denominator << "\n"; // будет мусор
}
```

Можно в `main()` сделать ее так:
```C++
Rational a{1, 2};
```
Это называется **агрегатной инициализацией**.  

Можно прямо в структуре определить значения переменных по умолчанию:  
```C++
struct Rational {
  int numerator = 0;
  int denominator = 1;
}
```

Сколько памяти будет занимать `Rational`?  
```C++
std::cout << sizeof(Rational) << "\n"; // будет 8 байт
```

Рассмотрим другой пример:
```C++
struct NewOrder {
  char side;
  double price;
  double volume;
};
```
Сколько памяти будет занимать `NewOrder`?  
```C++
std::cout << sizeof(NewOrder) << "\n"; // будет 24 байта, тк происходит выравнивание всего типа по максимальному полю
```
Если в эту функцию перед `char side` еще добавить `char company`, то все еще останется `24` байта.  
Если добавить после `double price` или `double volume`, то будет `32`.  

`#pragma pack(1)` на некоторых компиляторах помогает с тем, чтобы не тратить на это столько много памяти в приведенных выше ситуациях, но на `x86`, например, жертвуем производительностью.

Стоит отметить, что отменять выравнивание стоит лишь тогда, когда есть очень хорошее понимание, зачем эту нужно и какие будут последствия.
На более экзотических архитектурах доступ по невыравненному адресу может быть аппаратной ошибкой.

Внутри структуры можно определить функции:
```C++
struct Rational {
  void add(int x) {
    numerator += x * denominator;
  }
  int numeration = 0;  // в struct функции видят все переменные, что есть
  int denominator = 1; // хоть после них
};
```
Теперь в main() могу писать что-то такое:
```C++
Rational a;
a.add(10);
```

Функция внутри структуры с параметром=потоком вывода:
```C++
void print(std::ostream & strm) {
  strm << numerator << "/" << denominator << "\n";
}
```
Тогда в main() можно написать:
```C++
a.print(std::cout);
```

А можно сделать еще и так:  
```C++
std::ostream & print(std::ostream & strm) {
  strm << numerator << "/" << denominator << "\n";
}
```
Тогда в `main()` можно написать:  
```C++
a.print(std::cout) << "\n";
```

Поля класса можно записать в самом конце, и ничего не сломается.

Можно в структуре сделать объявление функции, а вне функции ее определить:  
```C++
struct Rational {
  int numerator;
  int denominator;
  void add(int x);
  void add(Rational x);
};
void Rational::add(int x) {
  numerator += x * denominator;
}
void Rational::add(const Rational x) {
  ...
}
```

Можно определить функцию:  
```C++
std::pair<Rational, bool> read_rational(std::istream & strm) {
  Rational r;
  char c;
  strm >> r.numerator >> c >> r.denominator;
  if (c == '/') {
    return {r, true};
  }
  return {{}, false};
}
```
Тогда в `main()`:  
```C++
auto res = read_rational(std::cin);
if (res.second) {
  res.first.print(std::count);
}
else {
  std::cout << "wrong value";
}
```

Но есть синтаксический сахар (начиная с `C++17`), который позволяет переписать `main()` на более интересный вариант:  
```C++
auto [r, ok] = read_rational(std::cin); // работает для любого агрегатного типа
if (ok) {
  r.print(std::cout);
}
else {
  std::cout << "wrong value";
}
```

Так как считывание `Rational` по смыслу привязано к его структуре, но не связано с каким-то объектом, можем объявить `read` в структуре, сделав его `static`. Теперь, чтобы его вызвать, пишем так `Rational::read(...)`;  
Статическую функцию можно вызвать и через объект.  
У `static` методов есть доступ ко всем объектам класса вне зависимости от прав доступа.  
Статические переменные-члены(поля) являются общими для всех объектов класса.  
Статические поля существуют, даже если объект класса не создан.  
Они создаются при запуске программы и уничтожаются при ее окончании. Ведут себя как глобальные переменные. Так как они не являются частью отдельных объектов класса, то нужно определять их вне тела класса - в глобальной области видимости.  

Можем объявит в классе `Rational` статическую переменную `static int variable;` и потом за классом определить ее `int Rational::variable = 3;`

Даже если эта переменная `private`, это определение возможно.  
Если же эта статическая переменная `constexpr`, ее можно определить в теле класса.  
Так же можно дописать слово `inline`, и тоже определить сразу. Тогда это будет аналогично глобальной `inline` переменной (можно включать в разные единицы трансляции, объявив в заголовочном файле).  
Если у вас просто статическая переменная, то если класс объявлен в заголовочном файле, определение переменной там писать нельзя, будет нарушение ОДР.  
Для методов класса (как статических, так и нет) слово `inline` подразумевается.   Поэтому определения методов можно включать в заголовочные файлы, и при подключении к разным единицам трансляции нарушения `ODR` не будет. `static` методы можно определять в теле класса.  


Если в `main` есть объект `const`, у него нельзя вызвать `non-const` метод, будет ошибка компиляции. Метка `const` означает, что `this` объект не будет изменен. Для `non-const` объектов `const` метод можно вызывать.  
Если в `const` методе попробовать изменить `this` (то есть поля), то будет ошибка компиляции.  
Если из `const` метода вызывать `non-const` метод, тоже будет ошибка.  
Хорошая практика писать `const` везде, где ничего не изменяется.  

```C++
bool compare(const Rational & x) const {
  return numerator == x.numerator && denominator == x.denominator;
}
```

Внутри можно переопределять оператор:  
```C++
bool operator ==(const Rational& a) const {
  return numerator == a.numerator && denominator == a.denominator;
}

Rational operator +(const Rational & a) const {
  auto tmp = *this;
  tmp.add(a);
  return tmp;
}

Rational & operator +=(const Rational& a) {
  numerator += a.numerator;
  denominator += a.denominator;
  return *this; // в структуре/классе this является указателем
}

Rational & operator ++ () // префиксный инкремент
{
  numerator += denominator;
  return *this;
}

Rational operator ++ (int) // постфиксный инкремент (именно int)
{
  auto tmp = *this;
  numerator += denominator;
  return tmp;
}
```

Можно переопределить оператор вывода. Проблема в том, что если мы хотим переопределить оператор внутри класса, объект класса должен передаваться первым параметром, но тут это сделать нельзя. Операторы можно перегружать как свободные функции, поэтому просто выносим объявление и определение во вне класса (в классе ничего писать не нужно).  
```C++
std::stream & operator << (std::stream & strm, const Rational x)
{
  return x.print(strm);
}
```

Если хотим, чтобы работала следующая операция `30 + r` (где r объект типа `Rational`), то нужно переопределить оператор как свободную функцию, так как опять же нельзя в классе переопределять метод, где объект не первый аргумент.  
Но не следует операцию типа `r + 30` определять внутри класса, чтобы избежать несимметричности. Лучше определять их как свободные функции, а уже "одиночные" методы объявлять как члены класса.  


## Про модификаторы доступа
Всего есть 3 модификатора доступа: `private`, `public`, `protected`:  
1) `private`: доступ имеет сам класс и все вложенные классы.  
2) `public`: все имеют доступ.  
3) `protected`: доступ имеет сам класс и все его потомки.  

```C++
struct Rational {
private:
  int numerator;
  int denominator;
public:
  ...
}
```
Если написать модификатор доступа, то все, что под ним, будет с таким модификатором доступа либо до конца структуры/класса, либо до нового модификатора доступа.

Свободные операторы не имеют доступ к приватным членам класса.

## Классы
Помимо `struct` есть `class`:
```C++
class Rational {
  ...
}
```
Они буквально взаимозаменяемые, есть лишь несколько отличий:
1) В `struct` члены по умолчанию `public`.  
2) В `class` члены по умолчанию `private`.  

В каком случае что использовать?
1) Если все члены доступа публичные, то лучше `struct`.
1) Если в качестве своих членов класс имеет только поля данных(нет функций и тд), то лучше `struct`.
1) В остальных случаях лучше `class`.

Еще один нюанс связан с наследованием:
```C++
struct X : Rational { // так мы сделали структуру X и наследовали ее от Rational
  ...                 // к слову наследование может быть множественным
}                     // права доступа наследуются
```

По умолчанию для `struct` наследование является `public`, те по умолчанию эквивалентно этому:
```C++
struct X : public Rational {
  ...
}
```
В `class` наследование по умолчанию приватное.



Обертка для моего типа и флага, где флаг показывает было ли проинициализировано мое значение `C++17`:
```C++
#include <optional>
std::optional<Rational> Rational::read(std::istream & strm)
{
  Rational r;
  char c;
  strm >> r.numerator >> c >> r.denominator;
  if (c == '/') {
    return r; // неявно приводит к нужному типу
  }
  return {};  // значение по умолчанию когда все плохо
}
int main() 
{
  auto r = Rational::read(std::cin);
  if (r) {
    r->print(std::cout) << "\n";
  } else {
    std::cout << "wrong value\n";
  }
}
// Чтобы извлечь объект целиком, пишем *r
```

Обычно используется если:
1) Нужно "красиво"(без использования `null`, `-1`) передать, что не получилось инициализировать мое значение.
2) Какое-то поле является опциональным, но хотелось бы знать ввел его пользователь или нет.
3) Вернуть результат каких-либо вычислений, которые не смогли дать конечный результат, но это не является.
   ошибкой (точно не стоит его использовать для обработки ошибок).
