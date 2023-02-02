#pragma once

#include <deque>
#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

class Searcher
{
public:
    using Word = std::string;
    using ID = std::size_t;
    using Position = std::size_t;
    using PositionSet = std::set<Position>;
    using IdToPositions = std::vector<std::pair<ID, PositionSet>>;

    using IdIterator = IdToPositions::const_iterator;

    using Filename = std::string;

    // index modification
    void add_document(const Filename & filename, std::istream & strm);
    void remove_document(const Filename & filename);

    // queries
    class DocIterator
    {
    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = const Filename;
        using pointer = const Filename *;
        using reference = const Filename &;

        DocIterator();
        DocIterator(const std::string & query, const Searcher * searcher);

        reference operator*() const;
        pointer operator->() const;
        DocIterator & operator++();
        DocIterator operator++(int);

        friend bool operator==(const DocIterator &, const DocIterator &);
        friend bool operator!=(const DocIterator &, const DocIterator &);

        bool isEnd() const;
        void dump(std::ostream & ostream, const std::string & comment = {}) const;

        //    private: fixme
        struct WordTerm
        {
        public:
            WordTerm(const Searcher * searcher, const Word & word);

            void next(Position lowerBound);
            bool isEnd() const;
            ID getCurrentId() const;

            const PositionSet & getPositions() const;
            bool containsPosition(Position pos) const;

            void dump(std::ostream & ostream, const std::string & comment = {}) const;

        private:
            IdToPositions::const_iterator it; // todo change to IdIterator
            IdToPositions::const_iterator end;
        };

        struct PhraseTerm
        {
        public:
            PhraseTerm(const Searcher * searcher, std::stringstream & str);
            PhraseTerm(const Searcher * searcher, const Word & word);

            bool empty() const;
            void next();
            bool isEnd() const;
            ID getCurrentId() const;

            bool checkDocument() const;
            bool checkLine(Position start) const;
            void makeEqual();

            void splitPhrase(const Searcher * search, std::stringstream & stream);
            void setEndAndId();

            void dump(std::ostream & ostream, const std::string & comment = {}) const;

        private:
            bool end;
            ID currentId;
            std::vector<WordTerm> words;
        };

        void querySplit(const std::string & query);
        void setEndAndId();
        void makeEqual();

        const Searcher * searcher;
        ID currentId;
        std::vector<PhraseTerm> terms;
    };

    class BadQuery : public std::exception
    {
    public:
        BadQuery(const std::string & message);
        virtual const char * what() const noexcept override;

    private:
        std::string message;
    };

    std::pair<DocIterator, DocIterator> search(const std::string & query) const;

    void dump(std::ostream & ostream, const std::string & comment = {}) const;

    //private: fixme
    class DocumentStorage
    {
    public:
        std::size_t size() const;

        ID topId();
        void addDocument(const Filename & path);
        ID removeDocument(const Filename & path);

        const Filename & operator[](ID id) const;
        bool contains(const Filename & doc) const;

        void dump(std::ostream & ostream, const std::string & comment = {}) const;

    private:
        std::set<ID> freeIds;
        std::vector<Filename> storage;
    };

    class Dictionary
    {
    public:
        std::size_t size() const;
        bool contains(const Word & word) const;

        IdToPositions::iterator findId(const Word & word, Searcher::ID id);
        void addToDict(const Word & word, ID id, Position position);
        void removeDoc(ID id);

        IdIterator docBegin(const Word & word) const;
        IdIterator docEnd(const Word & word) const;

        void dump(std::ostream & ostream, const std::string & comment = {}) const;

    private:
        const IdIterator emptyIterator = IdIterator();
        std::unordered_map<Word, IdToPositions> dictionary;
    };

    DocumentStorage documents;
    Dictionary dictionary;
};
