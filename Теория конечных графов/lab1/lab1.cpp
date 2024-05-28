#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <set>
#include <tuple>

enum class GraphRepresentation
{
    AdjacencyList,
    AdjacencyMatrix,
    ListOfEdges  
};

class AdjacencyMatrix;
class AdjacencyList;
class ListOfEdges;

class AdjacencyMatrix
{
private:
    std::vector<std::vector<int>> matrix;
public:
    bool isDirected;
    bool isWeighted;

    AdjacencyMatrix(bool isDirected = false, bool isWeighted = false, int verticesCount = 0);
    ~AdjacencyMatrix();
    void readGraph(std::string fileName);
    void readGraph(std::ifstream& in);
    void writeGraph(std::string fileName);
    void writeGraph(std::ofstream& out);

    void addEdge(int from, int to, int weight = 1);
    void removeEdge(int from, int to);
    int changeEdge(int from, int to, int newWeight);
    int getVerticesCount();
    int getEdgesCount();
    void clear();
    AdjacencyList transformToAdjList();
    ListOfEdges transformToListOfEdges();
};

class AdjacencyList
{
private:
    std::vector<std::set<std::pair<int, int>>> adjacencyList;
public:
    bool isDirected;
    bool isWeighted;

    AdjacencyList(bool isDirected = false, bool isWeighted = false, int verticesCount = 0);
    ~AdjacencyList();
    void readGraph(std::ifstream& in);
    void readGraph(std::string fileName);
    void writeGraph(std::ofstream& out);
    void writeGraph(std::string fileName);

    void addEdge(int from, int to, int weight = 1);
    void removeEdge(int from, int to);
    int changeEdge(int from, int to, int newWeight);
    int getVerticesCount();
    int getEdgesCount();
    void clear();
    AdjacencyMatrix transformToAdjMatrix();
    ListOfEdges transformToListOfEdges();
};

class ListOfEdges
{
private:
    std::vector<std::tuple<int, int, int>> listOfEdges;
public:
    bool isDirected;
    bool isWeighted;

    ListOfEdges(bool isDirected = false, bool isWeighted = false, int edgesCount = 0);
    ~ListOfEdges();
    void readGraph(std::ifstream& in);
    void readGraph(std::string fileName);
    void writeGraph(std::ofstream& out);
    void writeGraph(std::string fileName);

    void addEdge(int from, int to, int weight = 1);
    void removeEdge(int from, int to);
    int changeEdge(int from, int to, int newWeight);
    int getVerticesCount();
    int getEdgesCount();
    void clear();
    AdjacencyMatrix transformToAdjMatrix();
    AdjacencyList transformToAdjList();
};

class Graph
{
private:
    AdjacencyMatrix adjMatrix;
    AdjacencyList adjList;
    ListOfEdges listOfEdges;

    void readGraphIntroduction(std::string fileName);
    void readGraphIntroduction(std::ifstream& in);
    void writeGraphIntroduction(std::string fileName);
    void writeGraphIntroduction(std::ofstream& out);

public:
    int V;
    int E;
    bool isDirected;
    bool isWeighted;
    GraphRepresentation currentRepresentation;

    void readGraph(std::string fileName);
    void writeGraph(std::string fileName);
    void addEdge(int from, int to, int weight);
    void removeEdge(int from, int to);
    int changeEdge(int from, int to, int newWeight);
    void clear();
    void transformToAdjList();
    void transformToAdjMatrix();
    void transformToListOfEdges();
};


int main(int argc, char* argv[])
{
    std::string inputFileName = "input.txt";
    std::string outputFileName = "output.txt";

    Graph graph;
    graph.readGraph(inputFileName);
    GraphRepresentation inputGraphRepresentation = graph.currentRepresentation;
    
    graph.transformToAdjList();
    graph.transformToListOfEdges();
    if (graph.V <= 1e4)
    {
        graph.transformToAdjMatrix();
    }    
    graph.transformToAdjList();
    if (graph.V <= 1e4)
    {
        graph.transformToAdjMatrix();
    }    
    graph.transformToListOfEdges();

    if (inputGraphRepresentation == GraphRepresentation::AdjacencyMatrix)
    {
        graph.transformToAdjMatrix();
    }
    else if (inputGraphRepresentation == GraphRepresentation::AdjacencyList)
    {
        graph.transformToAdjList();
    }
    else
    {
        graph.transformToListOfEdges();
    }
    graph.writeGraph(outputFileName);
    graph.clear();
    return 0;
}

AdjacencyMatrix::AdjacencyMatrix(bool isDirected, bool isWeighted, int verticesCount)
{
    this->isDirected = isDirected;
    this->isWeighted = isWeighted;
    this->matrix.resize(verticesCount, std::vector<int>(verticesCount, 0));
}

AdjacencyMatrix::~AdjacencyMatrix()
{
    this->matrix.clear();
}

void AdjacencyMatrix::readGraph(std::ifstream& in)
{
    std::string line;
    while (std::getline(in, line))
    {
        std::vector<int> rowNumbers;
        std::string number;
        std::stringstream ss(line);
        while (!ss.eof())
        {
            std::getline(ss, number, ' ');
            rowNumbers.push_back(std::stoi(number));
        }
        this->matrix.push_back(rowNumbers);
    }
}

void AdjacencyMatrix::readGraph(std::string fileName)
{
    std::ifstream in(fileName);
    if (!in.is_open())
    {
        throw std::runtime_error("Cannot open file");
    }

    this->readGraph(in);
    in.close();
}

void AdjacencyMatrix::writeGraph(std::ofstream& out)
{
    for (int row = 0; row < this->matrix.size(); row++)
    {
        for (int column = 0; column < this->matrix[row].size(); column++)
        {
            out << this->matrix[row][column];
            if (column != this->matrix[row].size() - 1)
            {
                out << " ";
            }
        }
        out << "\n";
    }
}

void AdjacencyMatrix::writeGraph(std::string fileName)
{
    std::ofstream out(fileName);
    this->writeGraph(out);
    out.close();
}

void AdjacencyMatrix::addEdge(int from, int to, int weight)
{
    if (from > this->matrix.size())
    {
        this->matrix.resize(from);
        for (int i = 0; i < this->matrix.size(); i++)
        {
            this->matrix[i].resize(this->matrix.size());
        }
    }
    if (to > this->matrix.size())
    {
        this->matrix.resize(to);
        for (int i = 0; i < this->matrix.size(); i++)
        {
            this->matrix[i].resize(this->matrix.size());
        }
    }

    if (this->isDirected)
    {
        this->matrix[from-1][to-1] = weight;
    }
    else
    {
        this->matrix[from-1][to-1] = weight;
        this->matrix[to-1][from-1] = weight;
    }    
}

void AdjacencyMatrix::removeEdge(int from, int to)
{
    if (this->isDirected)
    {
        this->matrix[from-1][to-1] = 0;
    }
    else
    {
        this->matrix[from-1][to-1] = 0;
        this->matrix[to-1][from-1] = 0;
    }
}

int AdjacencyMatrix::changeEdge(int from, int to, int newWeight)
{
    int oldWeight = this->matrix[from-1][to-1];
    if (this->isDirected)
    {
        this->matrix[from-1][to-1] = newWeight;
    }
    else
    {
        this->matrix[from-1][to-1] = newWeight;
        this->matrix[to-1][from-1] = newWeight;
    }
    
    return oldWeight;
}

int AdjacencyMatrix::getVerticesCount()
{
    return this->matrix.size();
}

int AdjacencyMatrix::getEdgesCount()
{
    int edgesCount = 0;
    for (int row = 0; row < this->matrix.size(); row++)
    {
        for (int column = 0; column < this->matrix[row].size(); column++)
        {
            if (this->matrix[row][column] != 0)
            {
                edgesCount++;
            }
        }
    }

    return edgesCount;
}

void AdjacencyMatrix::clear()
{
    this->matrix.clear();
}

AdjacencyList AdjacencyMatrix::transformToAdjList()
{
    AdjacencyList adjList(this->isDirected, this->isWeighted);

    if (this->isDirected)
    {
        for (int row = 0; row < this->matrix.size(); row++)
        {
            for (int column = 0; column < this->matrix[row].size(); column++)
            {
                if (this->matrix[row][column] != 0)
                {
                    adjList.addEdge(row+1, column+1, this->matrix[row][column]);
                }
            }
        }
    }
    else
    {
        for (int row = 0; row < this->matrix.size(); row++)
        {
            for (int column = 0; column < this->matrix[row].size(); column++)
            {
                if (row > column)
                {
                    continue;
                }
                if (this->matrix[row][column] != 0)
                {
                    adjList.addEdge(row+1, column+1, this->matrix[row][column]);
                }
            }
        }
    }
    return adjList;
}

ListOfEdges AdjacencyMatrix::transformToListOfEdges()
{
    ListOfEdges listOfEdges(this->isDirected, this->isWeighted);
    
    if (this->isDirected)
    {
        for (int row = 0; row < this->matrix.size(); row++)
        {
            for (int column = 0; column < this->matrix[row].size(); column++)
            {
                if (this->matrix[row][column] != 0)
                {
                    listOfEdges.addEdge(row+1, column+1, this->matrix[row][column]);
                }
            }
        }
    }
    else
    {
        for (int row = 0; row < this->matrix.size(); row++)
        {
            for (int column = 0; column < this->matrix[row].size(); column++)
            {
                if (row > column)
                {
                    continue;
                }
                if (this->matrix[row][column] != 0)
                {
                    listOfEdges.addEdge(row+1, column+1, this->matrix[row][column]);
                }
            }
        }
    }
    return listOfEdges;
}

AdjacencyList::AdjacencyList(bool isDirected, bool isWeighted, int verticesCount)
{
    this->isDirected = isDirected;
    this->isWeighted = isWeighted;
    this->adjacencyList.resize(verticesCount);
}

AdjacencyList::~AdjacencyList()
{
    this->adjacencyList.clear();
}

void AdjacencyList::readGraph(std::ifstream& in)
{
    std::string line;
    while (std::getline(in, line))
    {
        std::set<std::pair<int, int>> rowNumbers;
        std::string number;
        std::string weight;
        std::stringstream ss(line);
        while (!ss.eof())
        {
            if (!this->isWeighted)
            {
                std::getline(ss, number, ' ');
                rowNumbers.insert(std::make_pair(std::stoi(number), 1));
            }
            else
            {
                std::getline(ss, number, ' ');
                std::getline(ss, weight, ' ');
                rowNumbers.insert(std::make_pair(std::stoi(number), std::stoi(weight)));
            }            
        }
        this->adjacencyList.push_back(rowNumbers);
    }
}

void AdjacencyList::readGraph(std::string fileName)
{
    std::ifstream in(fileName);
    if (!in.is_open())
    {
        throw std::runtime_error("Cannot open file");
    }

    this->readGraph(in);
    in.close();
}

void AdjacencyList::writeGraph(std::ofstream& out)
{
    for (int vertex = 0; vertex < this->adjacencyList.size(); vertex++)
    {
        for (auto edge : this->adjacencyList[vertex])
        {
            if (!this->isWeighted)
            {
                out << edge.first;
            }
            else
            {
                out << edge.first << " " << edge.second;
            }
            if (edge != *(this->adjacencyList[vertex].rbegin()))
            {
                out << " ";
            }
        }
        out << "\n";
    }
}

void AdjacencyList::writeGraph(std::string fileName)
{
    std::ofstream out(fileName);
    this->writeGraph(out);
    out.close();
}

void AdjacencyList::addEdge(int from, int to, int weight)
{
    if (from > this->adjacencyList.size())
    {
        this->adjacencyList.resize(from);
    }
    if (to > this->adjacencyList.size())
    {
        this->adjacencyList.resize(to);
    }
    if (this->isDirected)
    {
        this->adjacencyList[from-1].insert(std::make_pair(to, weight));
    }
    else
    {
        this->adjacencyList[from-1].insert(std::make_pair(to, weight));
        this->adjacencyList[to-1].insert(std::make_pair(from, weight));
    }
}

void AdjacencyList::removeEdge(int from, int to)
{    
    for (auto edge : this->adjacencyList[from-1])
    {
        if (edge.first == to)
        {
            this->adjacencyList[from-1].erase(edge);
            break;
        }
    }
    if (!this->isDirected)
    {
        for (auto edge : this->adjacencyList[to-1])
        {
            if (edge.first == from)
            {
                this->adjacencyList[to-1].erase(edge);
                break;
            }
        }
    }    
}

int AdjacencyList::changeEdge(int from, int to, int newWeight)
{
    int oldWeight;
    for (auto edge : this->adjacencyList[from-1])
    {
        if (edge.first == to)
        {
            oldWeight = edge.second;
            edge.second = newWeight;
            break;
        }
    }
    if (!this->isDirected)
    {
        for (auto edge : this->adjacencyList[to-1])
        {
            if (edge.first == from)
            {
                edge.second = newWeight;
                break;
            }
        }
    }
    
    return oldWeight;
}

int AdjacencyList::getVerticesCount()
{
    return this->adjacencyList.size();
}

int AdjacencyList::getEdgesCount()
{
    int edgesCount = 0;
    std::set<std::set<int>> traversedEdges;
    for (int row = 0; row < this->adjacencyList.size(); row++)
    {
        for (auto edge : this->adjacencyList[row])
        {
            std::set<int> edgeSet = {row+1, edge.first};
            if (traversedEdges.count(edgeSet) == 0)
            {
                edgesCount++;
            }
            traversedEdges.insert(edgeSet);
        }
    }

    return edgesCount;
}

void AdjacencyList::clear()
{
    this->adjacencyList.clear();
}

AdjacencyMatrix AdjacencyList::transformToAdjMatrix()
{
    AdjacencyMatrix matrix(this->isDirected, this->isWeighted, this->getVerticesCount());

    if (this->isDirected)
    {
        for (int row = 0; row < this->adjacencyList.size(); row++)
        {
            for (auto edge : this->adjacencyList[row])
            {
                matrix.addEdge(row+1, edge.first, edge.second);
            }
        }
    }
    else
    {
        std::set<std::set<int>> traversedEdges;
        for (int row = 0; row < this->adjacencyList.size(); row++)
        {
            for (auto edge : this->adjacencyList[row])
            {
                std::set<int> edgeSet = {row+1, edge.first};
                if (traversedEdges.count(edgeSet) == 0)
                {
                    matrix.addEdge(row+1, edge.first, edge.second);
                }
                traversedEdges.insert(edgeSet);
            }
        }
    }

    return matrix;
}

ListOfEdges AdjacencyList::transformToListOfEdges()
{
    ListOfEdges listOfEdges(this->isDirected, this->isWeighted);

    if (this->isDirected)
    {
        for (int row = 0; row < this->adjacencyList.size(); row++)
        {
            for (auto edge : this->adjacencyList[row])
            {
                listOfEdges.addEdge(row+1, edge.first, edge.second);
            }
        }
    }
    else
    {
        std::set<std::set<int>> traversedEdges;
        for (int row = 0; row < this->adjacencyList.size(); row++)
        {
            for (auto edge : this->adjacencyList[row])
            {
                std::set<int> edgeSet = {row+1, edge.first};
                if (traversedEdges.count(edgeSet) == 0)
                {
                    listOfEdges.addEdge(row+1, edge.first, edge.second);
                }
                traversedEdges.insert(edgeSet);
            }
        }
    }

    return listOfEdges;
}

ListOfEdges::ListOfEdges(bool isDirected, bool isWeighted, int edgesCount)
{
    this->isDirected = isDirected;
    this->isWeighted = isWeighted;
    this->listOfEdges.resize(edgesCount);
}

ListOfEdges::~ListOfEdges()
{
    this->listOfEdges.clear();
}

void ListOfEdges::readGraph(std::ifstream& in)
{
    std::string line;
    while (std::getline(in, line))
    {
        std::string from;
        std::string to;
        std::string weight;
        std::stringstream ss(line);
        std::getline(ss, from, ' ');
        std::getline(ss, to, ' ');
        
        if (this->isWeighted)
        {
            std::getline(ss, weight, ' ');
        }
        else
        {
            weight = "1";
        }
        
        std::tuple<int, int, int> edge = std::make_tuple(std::stoi(from), std::stoi(to), std::stoi(weight));
        this->listOfEdges.push_back(edge);
    }
}

void ListOfEdges::readGraph(std::string fileName)
{
    std::ifstream in(fileName);
    if (!in.is_open())
    {
        throw std::runtime_error("Cannot open file");
    }

    this->readGraph(in);
    in.close();
}

void ListOfEdges::writeGraph(std::ofstream& out)
{
    for (auto edge : this->listOfEdges)
    {
        out << std::get<0>(edge) << " " << std::get<1>(edge);
        if (this->isWeighted)
        {
            out << " " << std::get<2>(edge);
        }
        out << "\n";
    }
}

void ListOfEdges::writeGraph(std::string fileName)
{
    std::ofstream out(fileName);
    this->writeGraph(out);
    out.close();
}

void ListOfEdges::addEdge(int from, int to, int weight)
{
    std::tuple<int, int, int> edge = std::make_tuple(from, to, weight);
    
    this->listOfEdges.push_back(edge);
}

void ListOfEdges::removeEdge(int from, int to)
{
    for (int i = 0; i < this->listOfEdges.size(); i++)
    {
        std::tuple<int, int, int> edge = this->listOfEdges[i];
        if (this->isDirected)
        {
            if (std::get<0>(edge) == from && std::get<1>(edge) == to)
            {
                this->listOfEdges.erase(this->listOfEdges.begin() + i);
                break;
            }
        }
        else
        {
            if (std::get<0>(edge) == from && std::get<1>(edge) == to)
            {
                this->listOfEdges.erase(this->listOfEdges.begin() + i);
            }
            if (std::get<0>(edge) == to && std::get<1>(edge) == from)
            {
                this->listOfEdges.erase(this->listOfEdges.begin() + i);
            }
        }
    }
}

int ListOfEdges::changeEdge(int from, int to, int newWeight)
{
    int oldWeight;
    for (int i = 0; i < this->listOfEdges.size(); i++)
    {
        std::tuple<int, int, int> edge = this->listOfEdges[i];
        if (this->isDirected)
        {
            if (std::get<0>(edge) == from && std::get<1>(edge) == to)
            {
                oldWeight = std::get<2>(edge);
                std::get<2>(edge) = newWeight;
            }
        }
        else
        {
            if (std::get<0>(edge) == from && std::get<1>(edge) == to)
            {
                oldWeight = std::get<2>(edge);
                std::get<2>(edge) = newWeight;
            }
            else if (std::get<0>(edge) == to && std::get<1>(edge) == from)
            {
                std::get<2>(edge) = newWeight;
            }
        }
    }
    return oldWeight;
}

int ListOfEdges::getVerticesCount()
{
    std::set<int> vertices;
    for (auto edge : this->listOfEdges)
    {
        vertices.insert(std::get<0>(edge));
        vertices.insert(std::get<1>(edge));
    }

    return vertices.size();
}

int ListOfEdges::getEdgesCount()
{
    return this->listOfEdges.size();
}

void ListOfEdges::clear()
{
    this->listOfEdges.clear();
}

AdjacencyMatrix ListOfEdges::transformToAdjMatrix()
{
    AdjacencyMatrix matrix(this->isDirected, this->isWeighted, this->getVerticesCount());

    for (auto edge : this->listOfEdges)
    {
        matrix.addEdge(std::get<0>(edge), std::get<1>(edge), std::get<2>(edge));
    }

    return matrix;
}

AdjacencyList ListOfEdges::transformToAdjList()
{
    AdjacencyList list(this->isDirected, this->isWeighted);

    for (auto edge : this->listOfEdges)
    {
        list.addEdge(std::get<0>(edge), std::get<1>(edge), std::get<2>(edge));
    }

    return list;
}

// Граф

void Graph::readGraphIntroduction(std::ifstream& in)
{
    std::string firstLine;
    std::getline(in, firstLine);
    
    std::vector<std::string> firstLineTokens;
    std::stringstream ss(firstLine);
    std::string token;
    while (!ss.eof())
    {
        std::getline(ss, token, ' ');
        firstLineTokens.push_back(token);
    }
    if (firstLineTokens.size() != 2 && firstLineTokens.size() != 3)
    {
        throw std::runtime_error("Invalid input in 1 string");
    }

    if (firstLineTokens[0] == "C")
    {
        this->currentRepresentation = GraphRepresentation::AdjacencyMatrix;
        this->V = std::stoi(firstLineTokens[1]);
    }
    else if (firstLineTokens[0] == "L")
    {
        this->currentRepresentation = GraphRepresentation::AdjacencyList;
        this->V = std::stoi(firstLineTokens[1]);
    }
    else if (firstLineTokens[0] == "E")
    {
        this->currentRepresentation = GraphRepresentation::ListOfEdges;
        this->V = std::stoi(firstLineTokens[1]);
        this->E = std::stoi(firstLineTokens[2]);
    }
    else
    {
        throw std::invalid_argument("Invalid input in 1 string");
    }

    std::string secondLine;
    std::getline(in, secondLine);
    ss = std::stringstream(secondLine);
    std::string firstNumber;
    std::getline(ss, firstNumber, ' ');
    std::string secondNumber;
    std::getline(ss, secondNumber, ' ');
    if (firstNumber == "0") {
        this->isDirected = false;
    }
    else if (firstNumber == "1") {
        this->isDirected = true;
    }
    else {
        throw std::invalid_argument("Invalid input in 2 string");
    }

    if (secondNumber == "0") {
        this->isWeighted = false;
    }
    else if (secondNumber == "1") {
        this->isWeighted = true;
    }
    else {
        throw std::invalid_argument("Invalid input in 2 string");
    }
}

void Graph::readGraphIntroduction(std::string fileName)
{
    std::ifstream in(fileName);

    if (!in.is_open())
    {
        throw std::runtime_error("Cannot open file");
    }

    this->readGraphIntroduction(in);
    in.close();
}

void Graph::writeGraphIntroduction(std::ofstream& out)
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            out << "C " << this->V << std::endl;
            break;
        case GraphRepresentation::AdjacencyList:
            out << "L " << this->V << std::endl;
            break;
        case GraphRepresentation::ListOfEdges:
            out << "E " << this->V << " " << this->listOfEdges.getEdgesCount() << std::endl;
            break;
    }

    if (this->isDirected)
    {
        out << "1 ";
    }
    else
    {
        out << "0 ";
    }
    if (this->isWeighted)
    {
        out << "1" << std::endl;
    }
    else
    {
        out << "0" << std::endl;
    }
}

void Graph::writeGraphIntroduction(std::string fileName)
{
    std::ofstream out(fileName);
    this->writeGraphIntroduction(out);    
    out.close();
}

void Graph::readGraph(std::string fileName)
{
    std::ifstream in(fileName);
    if (!in.is_open())
    {
        throw std::runtime_error("Cannot open input file");
    }

    this->readGraphIntroduction(in);
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->adjMatrix.isDirected = this->isDirected;
            this->adjMatrix.isWeighted = this->isWeighted;
            this->adjMatrix.readGraph(in);
            break;
        case GraphRepresentation::AdjacencyList:
            this->adjList.isDirected = this->isDirected;
            this->adjList.isWeighted = this->isWeighted;
            this->adjList.readGraph(in);
            break;
        case GraphRepresentation::ListOfEdges:
            this->listOfEdges.isDirected = this->isDirected;
            this->listOfEdges.isWeighted = this->isWeighted;
            this->listOfEdges.readGraph(in);
            break;
    }
    in.close();
}

void Graph::writeGraph(std::string fileName)
{
    std::ofstream out(fileName);
    this->writeGraphIntroduction(out);
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->adjMatrix.writeGraph(out);
            break;
        case GraphRepresentation::AdjacencyList:
            this->adjList.writeGraph(out);
            break;
        case GraphRepresentation::ListOfEdges:
            this->listOfEdges.writeGraph(out);
            break;
    }
    out.close();
}

void Graph::addEdge(int from, int to, int weight = 1)
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->adjMatrix.addEdge(from, to, weight);
            break;
        case GraphRepresentation::AdjacencyList:
            this->adjList.addEdge(from, to, weight);
            break;
        case GraphRepresentation::ListOfEdges:
            this->listOfEdges.addEdge(from, to, weight);
            break;
    }
}

void Graph::removeEdge(int from, int to)
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->adjMatrix.removeEdge(from, to);
            break;
        case GraphRepresentation::AdjacencyList:
            this->adjList.removeEdge(from, to);
            break;
        case GraphRepresentation::ListOfEdges:
            this->listOfEdges.removeEdge(from, to);
            break;
    }
}

int Graph::changeEdge(int from, int to, int newWeight)
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            return this->adjMatrix.changeEdge(from, to, newWeight);
        case GraphRepresentation::AdjacencyList:
            return this->adjList.changeEdge(from, to, newWeight);
        case GraphRepresentation::ListOfEdges:
            return this->listOfEdges.changeEdge(from, to, newWeight);
    }
    return -1;
}

void Graph::clear()
{
    this->adjMatrix.clear();
    this->adjList.clear();
    this->listOfEdges.clear();
}

void Graph::transformToAdjList()
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->adjList = this->adjMatrix.transformToAdjList();
            this->adjMatrix.clear();
            this->currentRepresentation = GraphRepresentation::AdjacencyList;
            break;
        case GraphRepresentation::AdjacencyList:
            break;
        case GraphRepresentation::ListOfEdges:
            this->adjList = this->listOfEdges.transformToAdjList();
            this->listOfEdges.clear();
            this->currentRepresentation = GraphRepresentation::AdjacencyList;
            break;
    }
}

void Graph::transformToAdjMatrix()
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            break;
        case GraphRepresentation::AdjacencyList:
            this->adjMatrix = this->adjList.transformToAdjMatrix();
            this->adjList.clear();
            this->currentRepresentation = GraphRepresentation::AdjacencyMatrix;
            break;
        case GraphRepresentation::ListOfEdges:
            this->adjMatrix = this->listOfEdges.transformToAdjMatrix();
            this->listOfEdges.clear();
            this->currentRepresentation = GraphRepresentation::AdjacencyMatrix;
            break;
    }
}

void Graph::transformToListOfEdges()
{
    switch (this->currentRepresentation)
    {
        case GraphRepresentation::AdjacencyMatrix:
            this->listOfEdges = this->adjMatrix.transformToListOfEdges();
            this->adjMatrix.clear();
            this->currentRepresentation = GraphRepresentation::ListOfEdges;
            break;
        case GraphRepresentation::AdjacencyList:
            this->listOfEdges = this->adjList.transformToListOfEdges();
            this->adjList.clear();
            this->currentRepresentation = GraphRepresentation::ListOfEdges;
            break;
        case GraphRepresentation::ListOfEdges:
            break;
    }
}