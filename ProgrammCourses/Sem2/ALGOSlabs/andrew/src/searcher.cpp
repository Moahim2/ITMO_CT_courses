#include "searcher.h"

#include <algorithm>
#include <iostream>

namespace {

void message(std::ostream & ostream, const std::string & msg)
{
    ostream << msg << std::endl;
}

void message(const std::string & msg)
{
    message(std::cout, msg);
}

void section(std::ostream & ostream, bool bold, const std::string & title, const std::string & comment)
{
    if (bold) {
        ostream << "==================================================" << std::endl;
    }
    else {
        ostream << "--------------------------------------------------" << std::endl;
    }
    ostream << title << ": (" << comment << ")" << std::endl;
}

template <class T>
void printSet(std::ostream & ostream, const std::set<T> & set)
{
    ostream << "{ ";
    if (!set.empty()) {
        ostream << *set.begin();
        for (auto it = ++set.begin(); it != set.end(); ++it) {
            ostream << ", " << *it;
        }
    }
    ostream << " }";
}

template <class T>
void printVector(std::ostream & ostream, const std::vector<T> & vector)
{
    ostream << "{ ";
    if (!vector.empty()) {
        ostream << *vector.begin();
        for (auto it = ++vector.begin(); it != vector.end(); ++it) {
            ostream << ", " << *it;
        }
    }
    ostream << " }";
}

void skipWS(std::istream & stream)
{
    char c;
    while (std::isspace(stream.peek()) != 0 && stream.get(c)) {
    }
}

void getWord(std::istream & stream, std::string & string)
{
    string.clear();
    char c;
    while (std::isspace(stream.peek()) == 0 && stream.peek() != '"' && stream.get(c)) {
        string.push_back(c);
    }
}

void trimWord(Searcher::Word & word)
{
    auto it = std::find_if(word.begin(), word.end(), [](char c) { return std::isalnum(static_cast<unsigned char>(c)) != 0; });
    word.erase(word.begin(), it);
    it = std::find_if(word.rbegin(), word.rend(), [](char c) { return std::isalnum(static_cast<unsigned char>(c)) != 0; }).base();
    word.erase(it, word.end());
    std::transform(word.begin(), word.end(), word.begin(), [](char c) { return std::tolower(static_cast<unsigned char>(c)); });
}

} // anonymous namespace

// ================================================================================================
// Document storage

std::size_t Searcher::DocumentStorage::size() const
{
    return storage.size();
}

Searcher::ID Searcher::DocumentStorage::topId()
{
    return freeIds.empty() ? storage.size() : *freeIds.begin();
}

void Searcher::DocumentStorage::addDocument(const Searcher::Filename & path)
{
    if (!freeIds.empty()) {
        auto id = *freeIds.begin();
        freeIds.erase(freeIds.begin());
        storage[id] = path;
    }
    else {
        storage.push_back(path);
    }
}

Searcher::ID Searcher::DocumentStorage::removeDocument(const Searcher::Filename & path)
{
    bool found = false;
    ID result;
    for (std::size_t i = 0; i < storage.size(); ++i) {
        if (freeIds.find(i) == freeIds.end() && storage[i] == path) {
            found = true;
            result = i;
            break;
        }
    }

    if (!found) {
        return storage.size();
    }

    freeIds.insert(result);
    return result;
}

const Searcher::Filename & Searcher::DocumentStorage::operator[](Searcher::ID id) const
{
    return storage.at(id);
}

bool Searcher::DocumentStorage::contains(const Filename & filename) const
{
    for (std::size_t i = 0; i < storage.size(); ++i) {
        if (freeIds.find(i) != freeIds.end()) { // todo change freeIds type to set
            continue;
        }

        if (storage[i] == filename) {
            return true;
        }
    }
    return false;
}

void Searcher::DocumentStorage::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, false, "DOCUMENT STORAGE", comment);
    ostream << "ID  |  PATH" << std::endl;
    for (std::size_t i = 0; i < storage.size(); ++i) {
        if (std::find(freeIds.begin(), freeIds.end(), i) != freeIds.end()) {
            continue;
        }
        ostream << i << "   |  " << storage[i] << std::endl;
    }
}

// ================================================================================================
// Dictionary

std::size_t Searcher::Dictionary::size() const
{
    return dictionary.size();
}

bool Searcher::Dictionary::contains(const Searcher::Word & word) const
{
    return dictionary.find(word) != dictionary.end();
}

Searcher::IdToPositions::iterator Searcher::Dictionary::findId(const Word & word, Searcher::ID id)
{
    return std::find_if(dictionary[word].begin(),
                        dictionary[word].end(),
                        [&](const std::pair<ID, PositionSet> & p) { return p.first == id; });
}

void Searcher::Dictionary::addToDict(const Searcher::Word & word,
                                     Searcher::ID id,
                                     Searcher::Position position)
{
    if (word.empty()) {
        return;
    }
    if (dictionary.find(word) == dictionary.end()) {
        dictionary.insert(std::make_pair(word, IdToPositions()));
    }

    if (findId(word, id) == dictionary[word].end()) {
        dictionary[word].emplace_back(id, PositionSet());
    }

    findId(word, id)->second.emplace(position);
}

void Searcher::Dictionary::removeDoc(ID id)
{
    auto pair = dictionary.begin();
    while (pair != dictionary.end()) {
        auto it = std::find_if(pair->second.begin(), pair->second.end(), [&](const std::pair<ID, PositionSet> & p) { return p.first == id; });
        if (it != pair->second.end()) {
            pair->second.erase(it);
        }
        auto check = pair;
        ++pair;
        if (check->second.empty()) {
            dictionary.erase(check);
        }
    }
}

Searcher::IdIterator Searcher::Dictionary::docBegin(const Word & word) const
{
    if (dictionary.find(word) == dictionary.end()) {
        return emptyIterator;
    }
    return dictionary.at(word).begin();
}

Searcher::IdIterator Searcher::Dictionary::docEnd(const Word & word) const
{
    if (dictionary.find(word) == dictionary.end()) {
        return emptyIterator;
    }
    return dictionary.at(word).end();
}

void Searcher::Dictionary::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, false, "DICTIONARY", comment);
    for (auto it = dictionary.begin(); it != dictionary.end(); ++it) {
        ostream << it->first << std::endl;
        for (auto it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
            ostream << "    ID " << it2->first << "  :  { " << *it2->second.begin();
            for (auto it3 = ++it2->second.begin(); it3 != it2->second.end(); ++it3) { // todo to function
                ostream << ", " << *it3;
            }
            ostream << " }" << std::endl;
        }
    }
}

// ------------------------------------------------------------------------------------------------
// WordTerm

Searcher::DocIterator::WordTerm::WordTerm(const Searcher * searcher, const Searcher::Word & word)
    : it(searcher->dictionary.docBegin(word))
    , end(searcher->dictionary.docEnd(word))
{
}

void Searcher::DocIterator::WordTerm::next(Position lowerBound)
{
    if (it->first >= lowerBound) {
        return;
    }
    std::size_t l = 0;
    std::size_t r = end - it;

    while (l < r - 1) {
        std::size_t m = (l + r) / 2;
        if ((it + m)->first < lowerBound) {
            l = m;
        }
        else {
            r = m;
        }
    }

    it += r;
}
bool Searcher::DocIterator::WordTerm::isEnd() const
{
    return it == end;
}
Searcher::ID Searcher::DocIterator::WordTerm::getCurrentId() const
{
    return it->first;
}

const Searcher::PositionSet & Searcher::DocIterator::WordTerm::getPositions() const
{
    return it->second;
}

bool Searcher::DocIterator::WordTerm::containsPosition(Searcher::Position pos) const
{
    if (Searcher::DocIterator::WordTerm::isEnd()) {
        return false;
    }
    return it->second.find(pos) != it->second.end();
}

void Searcher::DocIterator::WordTerm::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, false, "WORD ITERATOR", comment);
    ostream << "Current state: ";
    if (isEnd()) {
        ostream << "End of documents list reached";
    }
    else {
        ostream << "Id: " << it->first << " Positions: ";
        printSet(ostream, it->second);
    }
    ostream << std::endl;
}

// ------------------------------------------------------------------------------------------------
// PhraseTerm

Searcher::DocIterator::PhraseTerm::PhraseTerm(const Searcher * searcher, std::stringstream & str)
    : end(false)
    , currentId(0)
{
    splitPhrase(searcher, str);
    setEndAndId();
    makeEqual();
    if (!isEnd() && !checkDocument()) {
        next();
    }
}

Searcher::DocIterator::PhraseTerm::PhraseTerm(const Searcher * searcher, const Searcher::Word & word)
    : end(false)
    , currentId(0)
{
    words.emplace_back(searcher, word);
    setEndAndId();
}

bool Searcher::DocIterator::PhraseTerm::empty() const
{
    return words.empty();
}

void Searcher::DocIterator::PhraseTerm::next()
{
    do {
        currentId += 1;
        words.front().next(currentId);
        makeEqual();
    } while (!isEnd() && !checkDocument());
}

bool Searcher::DocIterator::PhraseTerm::isEnd() const
{
    return end;
}

Searcher::ID Searcher::DocIterator::PhraseTerm::getCurrentId() const
{
    return words.front().getCurrentId();
}

// You can use it only if all word iterators will point to the same document
bool Searcher::DocIterator::PhraseTerm::checkDocument() const
{
    auto first = words.front().getPositions().begin();
    auto last = words.front().getPositions().end();

    while (first != last) {
        if (checkLine(*first)) {
            return true;
        }
        else {
            ++first;
        }
    }
    return false;
}

bool Searcher::DocIterator::PhraseTerm::checkLine(Searcher::Position start) const
{
    for (auto it = words.begin(); it != words.end(); ++it) {
        if (!it->containsPosition(start++)) {
            return false;
        }
    }
    return true;
}

void Searcher::DocIterator::PhraseTerm::makeEqual()
{
    if (words.empty() || Searcher::DocIterator::PhraseTerm::isEnd()) {
        return;
    }

    std::size_t sizeCheck = 0;
    std::size_t currentWord = 0;

    while (!words[currentWord].isEnd() && sizeCheck < words.size()) {
        if (words[currentWord].getCurrentId() == currentId) {
            ++sizeCheck;
            currentWord = currentWord + 1 < words.size() ? currentWord + 1 : 0;
        }
        else if (words[currentWord].getCurrentId() < currentId) {
            words[currentWord].next(currentId);
        }
        else {
            sizeCheck = 0;
            currentId = words[currentWord].getCurrentId();
        }
    }
    if (sizeCheck < words.size()) {
        end = true;
    }
}

void Searcher::DocIterator::PhraseTerm::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, true, "PHRASE ITERATOR", comment);
    ostream << "Is end: " << isEnd() << std::endl;
    for (auto it = words.begin(); it != words.end(); ++it) {
        it->dump(ostream, comment);
    }
}

void Searcher::DocIterator::PhraseTerm::splitPhrase(const Searcher * search, std::stringstream & stream)
{
    if (stream.peek() != '"') {
        throw std::runtime_error("Phrase does not start with `\"`");
    }

    stream.get(); // get quote
    skipWS(stream);
    Word word;

    while (stream.peek() != '"' && !stream.eof()) {
        getWord(stream, word);
        trimWord(word);
        if (!word.empty()) {
            words.emplace_back(search, word);
        }
        skipWS(stream);
    }
    if (stream.eof()) {
        throw BadQuery("Query contains unmatched quotes");
    }

    stream.get(); // get quote

    if (empty()) {
        end = true;
    }

    //    if (!empty() && !words.at(0).isEnd()) {
    //        currentId = words.at(0).getCurrentId();
    //    }
    //    else {
    //        end = true;
    //    }
}

void Searcher::DocIterator::PhraseTerm::setEndAndId()
{
    for (auto it = words.begin(); it != words.end(); ++it) {
        if (it->isEnd()) {
            end = true;
            break;
        }
        currentId = std::max(currentId, it->getCurrentId());
    }
}

// ================================================================================================
// Doc Iterator

Searcher::DocIterator::DocIterator()
    : searcher(nullptr)
    , currentId(0)
{
}

Searcher::DocIterator::DocIterator(const std::string & query, const Searcher * searcher)
    : searcher(searcher)
    , currentId(0)
{
    querySplit(query);
    setEndAndId();
    makeEqual();
}

const Searcher::Filename & Searcher::DocIterator::operator*() const
{
    return searcher->documents[currentId];
}

Searcher::DocIterator::pointer Searcher::DocIterator::operator->() const
{
    return &operator*();
}

Searcher::DocIterator & Searcher::DocIterator::operator++()
{
    terms.at(0).next();
    makeEqual();
    return *this;
}

Searcher::DocIterator Searcher::DocIterator::operator++(int)
{
    auto result = *this;
    operator++();
    return result;
}

bool operator==(const Searcher::DocIterator & a, const Searcher::DocIterator & b)
{
    return (a.searcher == nullptr && b.searcher == nullptr) || (a.searcher == b.searcher && a.currentId == b.currentId);
}

bool operator!=(const Searcher::DocIterator & a, const Searcher::DocIterator & b)
{
    return !(a == b);
}

bool Searcher::DocIterator::isEnd() const
{
    return searcher == nullptr;
}

void Searcher::DocIterator::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, true, "DOCUMENT ITERATOR", comment);
    for (auto it = terms.begin(); it != terms.end(); ++it) {
        it->dump(ostream, comment);
    }
}

void Searcher::DocIterator::querySplit(const std::string & query)
{
    std::stringstream stream(query);
    Word word;
    skipWS(stream);
    while (!stream.eof()) {
        if (stream.peek() == '"') {
            PhraseTerm phrase(searcher, stream);
            if (!phrase.empty()) {
                terms.push_back(phrase);
            }
        }
        else {
            getWord(stream, word);
            trimWord(word);
            if (!word.empty()) {
                terms.emplace_back(searcher, word);
            }
        }
        skipWS(stream);
    }

    if (terms.empty()) {
        throw BadQuery("Empty query");
    }
    if (terms.at(0).isEnd()) {
        searcher = nullptr;
    }
    else {
        currentId = terms.at(0).getCurrentId();
    }
}

void Searcher::DocIterator::makeEqual()
{
    
    if (isEnd()) {
        return;
    }

    std::size_t sizeCheck = 0;
    std::size_t currentWord = 0;

    while (!terms.at(currentWord).isEnd() && sizeCheck < terms.size()) {
        if (terms.at(currentWord).getCurrentId() == currentId) {
            ++sizeCheck;
            currentWord = currentWord + 1 < terms.size() ? currentWord + 1 : 0;
        }
        else if (terms.at(currentWord).getCurrentId() < currentId) {
            terms.at(currentWord).next();
        }
        else {
            sizeCheck = 0;
            currentId = terms.at(currentWord).getCurrentId();
        }
    }
    if (sizeCheck < terms.size()) {
        searcher = nullptr;
    }
}

void Searcher::DocIterator::setEndAndId()
{
    for (auto it = terms.begin(); it != terms.end(); ++it) {
        if (it->isEnd()) {
            searcher = nullptr;
            break;
        }
        currentId = std::max(currentId, it->getCurrentId());
    }
}

// ================================================================================================
// Bad Query

Searcher::BadQuery::BadQuery(const std::string & message)
    : message(message)
{
}

const char * Searcher::BadQuery::what() const noexcept
{
    return message.c_str();
}

// ================================================================================================
// Searcher

void Searcher::add_document(const Searcher::Filename & filename, std::istream & strm)
{
    if (documents.contains(filename)) {
        remove_document(filename);
    }
    ID id = documents.topId();
    documents.addDocument(filename);

    Position position = 0;
    Word currentWord;
    while (strm >> currentWord) {
        trimWord(currentWord);
        if (!currentWord.empty()) {
            dictionary.addToDict(currentWord, id, position++);
        }
    }
}

void Searcher::remove_document(const Searcher::Filename & filename)
{
    if (documents.contains(filename)) {
        ID id = documents.removeDocument(filename);
        dictionary.removeDoc(id);
    }
}

void Searcher::dump(std::ostream & ostream, const std::string & comment) const
{
    section(ostream, true, "INDEX", comment);

    ostream << "Document storage size: " << documents.size() << std::endl;
    ostream << "Dictionary size: " << dictionary.size() << std::endl;

    documents.dump(ostream, comment);
    dictionary.dump(ostream, comment);
}

std::pair<Searcher::DocIterator, Searcher::DocIterator> Searcher::search(const std::string & query) const
{
    return std::make_pair(DocIterator(query, this), DocIterator());
}
