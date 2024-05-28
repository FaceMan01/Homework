#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <tuple>
#include <iterator>
#include <stack>
#include <queue>
#include <map>
#include <algorithm>
#include <functional>
 
using namespace std;
 
class RepresType 
{
protected:
    int vertex_num = 0;
    bool is_weighted;
    bool is_oriented;
public:
    virtual void readGraph(istream & ist) = 0;
    virtual void addEdge(int from, int to, int weight = 1) = 0;
    virtual void removeEdge(int from, int to) = 0;
    virtual int changeEdge(int from, int to, int newWeight) = 0;
    virtual vector<vector<int>> transformToAdjMatrix() = 0;
    virtual vector<set<pair<int, int>>> transformToAdjList() = 0;
    virtual vector <tuple<int, int, int>> transformToListOfEdges() = 0;
    virtual void writeGraph(string fileName) = 0;
    virtual tuple<bool, bool, int> GetInfo() = 0;
};
 
struct Edge {
    int flow;
    int cap;
};
 
vector<string> SplitingTheString(string str, char delim) 
{
    vector<string> result;
    string cur_strg = "";
    for (int i = 0; i < str.size(); i++) {
        if (str[i] != delim)
            cur_strg.push_back(str[i]);
        else {
            if (cur_strg != "")
                result.push_back(cur_strg);
            cur_strg = "";
        }
    }
    if (cur_strg != "")
        result.push_back(cur_strg);
    return result;
}

void QuickSorting(vector<tuple<int, int, int>> &mas, int first, int last)
{
    int middle;
    tuple<int, int, int> count;
    int f = first, l = last;
    middle = get<2>(mas[(f + l) / 2]);
    do
    {
        while (get<2>(mas[f])<middle) f++;
        while (get<2>(mas[l])>middle) l--;
        if (f <= l)
        {
            count = mas[f];
            mas[f] = mas[l];
            mas[l] = count;
            f++;
            l--;
        }
    } while (f < l);
    if (first < l) QuickSorting(mas, first, l);
    if (f < last) QuickSorting(mas, f, last);
}
 
class AdjMatrixGraph :public RepresType {
private:
    vector<vector<int>> adj_matrix;
public:
   
    AdjMatrixGraph() {
       
    }
   
    AdjMatrixGraph(int N) {
        vertex_num = N;
        for (int i = 0; i < N; i++) {
            vector<int> vertex;
            for (int j = 0; j < N; j++)
                vertex.push_back(0);
            adj_matrix.push_back(vertex);
        }
    }
   
    AdjMatrixGraph(vector<vector<int>> matrix, tuple<bool, bool, int> info) {
        adj_matrix = matrix;
        is_weighted = get<0>(info);
        is_oriented = get<1>(info);
        vertex_num = get<2>(info);
    }
   
    void readGraph(istream & ist) override {
        ist >> vertex_num;
        ist >> is_oriented;
        ist >> is_weighted;
        for (int i = 0; i < vertex_num; i++) {
            vector<int> row;
            for (int j = 0; j < vertex_num; j++) {
                int cell;
                ist >> cell;
                row.push_back(cell);
            }
            adj_matrix.push_back(row);
        }
    }
   
    void addEdge(int from, int to, int weight = 1) override {
        adj_matrix[from - 1][to - 1] = weight;
        if (!is_oriented)
            adj_matrix[to - 1][from - 1] = weight;
    }
   
    void removeEdge(int from, int to) override {
        adj_matrix[from - 1][to - 1] = 0;
        adj_matrix[to - 1][from - 1] = 0;
    }
   
    int changeEdge(int from, int to, int newWeight) override {
        int weight = adj_matrix[from - 1][to - 1];
        adj_matrix[to - 1][from - 1] = newWeight;
        if (!is_oriented)
            adj_matrix[from - 1][to - 1] = newWeight;
        return weight;
    }
   
    vector<vector<int>> transformToAdjMatrix() override {
        return adj_matrix;
    }
   
    vector<set<pair<int, int>>> transformToAdjList() override {
        vector<set<pair<int, int>>> adj_list;
        for (int i = 0; i < vertex_num; i++) {
            set<pair<int, int>> vertex;
            for (int j = 0; j < vertex_num; j++) {
                if (adj_matrix[i][j] > 0) {
                    vertex.insert(pair<int, int>(j + 1, adj_matrix[i][j]));
                }
            }
            adj_list.push_back(vertex);
        }
        return adj_list;
    }
   
    vector <tuple<int, int, int>> transformToListOfEdges() override {
        vector<tuple<int, int, int>> list_of_edges;
        for (int i = 0; i < vertex_num; i++) {
            for (int j = 0; j < vertex_num; j++) {
                if (adj_matrix[i][j] > 0) {
                    list_of_edges.push_back(tuple<int, int, int>(i + 1, j + 1, adj_matrix[i][j]));
                    if (!is_oriented)
                        adj_matrix[j][i] = 0;
                }
            }
        }
        return list_of_edges;
    }
   
    void writeGraph(string fileName) override {
        ofstream file(fileName);
        for (int i = 0; i < vertex_num; i++) {
            for (int j = 0; j < vertex_num; j++)
                file << adj_matrix[i][j] << " ";
            file << endl;
        }
    }
   
    tuple<bool, bool, int> GetInfo() override {
        return tuple<bool, bool, int>(is_weighted, is_oriented, vertex_num);
    }
   
};
 
class AdjListGraph :public RepresType {
private:
    vector<set<pair<int, int>>> adj_list;
public:
    AdjListGraph() {
       
    }
   
    AdjListGraph(int n) {
        vertex_num = n;
        for (int i = 0; i < n; i++) {
            set<pair<int, int>> vertex;
            adj_list.push_back(vertex);
        }
    }
   
    AdjListGraph(vector<set<pair<int, int>>> list, tuple<bool, bool, int> info) {
        adj_list = list;
        is_weighted = get<0>(info);
        is_oriented = get<1>(info);
        vertex_num = get<2>(info);
    }
   
    void readGraph(istream & ist) override {
        ist >> vertex_num;
        ist >> is_oriented;
        ist >> is_weighted;
        string cur_vertexList;
        getline(ist, cur_vertexList);
       
        if (is_weighted) {
            for (int i = 0; i < vertex_num; i++) {
                getline(ist, cur_vertexList);
                vector<string> neigh = SplitingTheString(cur_vertexList, ' ');
                set<pair<int, int>> adjanceciesOfVertex;
                for (int j = 0; j < neigh.size(); j += 2) {
                    pair<int, int> adjancecy(stoi(neigh[j]), stoi(neigh[j + 1]));
                    adjanceciesOfVertex.insert(adjancecy);
                }
                adj_list.push_back(adjanceciesOfVertex);
            }
        }
        else {
            for (int i = 0; i < vertex_num; i++) {
                getline(ist, cur_vertexList);
                vector<string> neigh = SplitingTheString(cur_vertexList, ' ');
                set<pair<int, int>> adjanceciesOfVertex;
                for (int j = 0; j < neigh.size(); j++) {
                    pair<int, int> adjancecy(stoi(neigh[j]), 1);
                    adjanceciesOfVertex.insert(adjancecy);
                }
                adj_list.push_back(adjanceciesOfVertex);
            }
        }
    }
   
    void addEdge(int from, int to, int weight = 1) override {
        if (!is_weighted) {
            adj_list[from - 1].insert(pair<int, int>(to, 1));
            if (!is_oriented)
                adj_list[to - 1].insert(pair<int, int>(from, 1));
        }
        else {
            adj_list[from - 1].insert(pair<int, int>(to, weight));
            if (!is_oriented)
                adj_list[to - 1].insert(pair<int, int>(from, weight));
        }
    }
   
    void removeEdge(int from, int to) override {
        for (auto iter = adj_list[from - 1].begin(); iter != adj_list[from - 1].end(); iter++)
            if ((iter->first) == to) {
                adj_list[from - 1].erase(iter);
                break;
            }
        if (!is_oriented) {
            for (auto iter = adj_list[to - 1].begin(); iter != adj_list[to - 1].end(); iter++)
                if ((iter->first) == from) {
                    adj_list[to - 1].erase(iter);
                    break;
                }
        }
    }
   
    int changeEdge(int from, int to, int newWeight) override {
        int weight = 0;
        for (auto iter = adj_list[from - 1].begin(); iter != adj_list[from - 1].end(); iter++)
            if ((iter->first) == to) {
                weight = iter->second;
                break;
            }
        this->removeEdge(from, to);
        this->addEdge(from, to, newWeight);
        if (!is_oriented) {
            this->removeEdge(to, from);
            this->addEdge(to, from, newWeight);
        }
        return weight;
    }
   
    vector<vector<int>> transformToAdjMatrix() override {
        vector<vector<int>> adj_matrix;
        for (int i = 0; i < vertex_num; i++) {
            vector<int> row;
            for (int j = 0; j < vertex_num; j++)
                row.push_back(0);
            adj_matrix.push_back(row);
        }
       
        if (!is_weighted) {
            for (int i = 0; i < vertex_num; i++) {
                for (auto iter = adj_list[i].begin(); iter != adj_list[i].end(); iter++) {
                    adj_matrix[i][iter->first - 1] = 1;
                    if (!is_oriented)
                        adj_matrix[iter->first - 1][i] = 1;
                    else
                        adj_matrix[iter->first - 1][i] = -1;
                }
            }
        }
        else {
            for (int i = 0; i < vertex_num; i++) {
                for (auto iter = adj_list[i].begin(); iter != adj_list[i].end(); iter++) {
                    adj_matrix[i][iter->first - 1] = iter->second;
                    if (!is_oriented)
                        adj_matrix[iter->first - 1][i] = iter->second;
                    else
                        adj_matrix[iter->first - 1][i] = -(iter->second);
                }
            }
        }
        return adj_matrix;
    }
   
    vector<set<pair<int, int>>> transformToAdjList() override {
        return adj_list;
    }
   
    vector <tuple<int, int, int>> transformToListOfEdges() override {
        vector<tuple<int, int, int>> list_of_edges;
        vector<pair<int, int>> vec;
        for (int i = 0; i < vertex_num; i++) {
            for (auto iter = adj_list[i].begin(); iter != adj_list[i].end(); iter++) {
                bool flag = true;
                for (int j = 0; j < vec.size(); j++)
                    if ((vec[j].first == iter->first) && (vec[j].second == i + 1))
                        flag = false;
                if (flag) {
                    list_of_edges.push_back(tuple<int, int, int>(i + 1, iter->first, iter->second));
                    vec.push_back(pair<int, int>(i + 1, iter->first));
                }
            }
        }
        return list_of_edges;
    }
   
    void writeGraph(string fileName) override {
        ofstream file(fileName);
        file << "L " << vertex_num << endl;
        file << is_oriented << " " << is_weighted;
        for (int i = 0; i < vertex_num; i++) {
            file << endl;
            for (auto iter = adj_list[i].begin(); iter != adj_list[i].end(); iter++) {
                if (iter != adj_list[i].begin()) file << " ";
                file << iter->first;
                if (is_weighted)
                    file << " " << iter->second;
            }
        }
        file.close();
    }
   
    tuple<bool, bool, int> GetInfo() override {
        return tuple<bool, bool, int>(is_weighted, is_oriented, vertex_num);
    }

    int checkBipart() {
        vector<int> marks;
        for (int i = 0; i < adj_list.size(); i++)
            marks.push_back(0);
        queue<int> q;
        q.push(0);
        marks[0] = 1;
        int len = 1;
        int current_mark = 1;
        while (!q.empty()) {
            int node = q.front();
            q.pop();
            current_mark *= -1;
            for (auto iter = adj_list[node].begin(); iter != adj_list[node].end(); iter++) {
                if (marks[iter->first - 1] == 0) {
                    marks[iter->first - 1] = current_mark;
                    q.push(iter->first - 1);
                    len++;
                }
                else {
                    if (marks[iter->first - 1] == current_mark) {
                        continue;
                    }
                    else
                        return 0;
                }
                if (len == vertex_num)
                    return len;
            }
        }
        if (len != vertex_num)
            return 0;
        return len;
    }
   
    bool TryKun(int v, vector<bool> &used, vector<int> &vec) {
        if (used[v])  return false;
        used[v] = true;
        for (auto iter = adj_list[v].begin(); iter != adj_list[v].end(); iter++) {
            int to = iter->first - 1;
            if (vec[to] == -1 || TryKun(vec[to], used, vec)) {
                vec[to] = v;
                return true;
            }
        }
        return false;
    }
   
    vector<pair<int, int> > getMaximumMatchingBipart() {
        vector<pair<int, int>> result;
        vector<int> vec;
        vector<bool> used;
        for (int i = 0; i < vertex_num; i++) {
            used.push_back(false);
            vec.push_back(-1);
        }
        for (int i = 0; i < vertex_num; i++)
            TryKun(i, used, vec);
       
        for (int i = 0; i < vertex_num; i++) {
            if (used[i]) {
                result.push_back(pair<int, int>(i + 1, vec[i] + 1));
                used[vec[i]] = false;
                used[i] = false;
            }
        }
        return result;
    }
};
 
class ListOfEdgesGraph :public RepresType {
private:
    int num_of_edges;
    vector <tuple<int, int, int>> list_of_edges;
public:
    ListOfEdgesGraph() {
       
    }
   
    ListOfEdgesGraph(int n) {
        vertex_num = n;
    }
   
    ListOfEdgesGraph(vector <tuple<int, int, int>> list, tuple<bool, bool, int> info) {
        list_of_edges = list;
        is_weighted = get<0>(info);
        is_oriented = get<1>(info);
        vertex_num = get<2>(info);
        num_of_edges = list.size();
    }
   
    void readGraph(istream & ist) override {
        ist >> vertex_num;
        ist >> num_of_edges;
        ist >> is_oriented;
        ist >> is_weighted;
        string cur_edge;
        getline(ist, cur_edge);
        if (is_weighted) {
            for (int i = 0; i < num_of_edges; i++) {
                getline(ist, cur_edge);
                vector<string> vertexes = SplitingTheString(cur_edge, ' ');
                list_of_edges.push_back(tuple<int, int, int>(stoi(vertexes[0]), stoi(vertexes[1]), stoi(vertexes[2])));
            }
        }
        else {
            for (int i = 0; i < num_of_edges; i++) {
                getline(ist, cur_edge);
                vector<string> vertexes = SplitingTheString(cur_edge, ' ');
                list_of_edges.push_back(tuple<int, int, int>(stoi(vertexes[0]), stoi(vertexes[1]), 1));
            }
        }
    }
   
    void addEdge(int from, int to, int weight = 1) override {
        if (!is_weighted) {
            list_of_edges.push_back(tuple<int, int, int>(from, to, 1));
        }
        else {
            list_of_edges.push_back(tuple<int, int, int>(from, to, weight));
        }
        num_of_edges++;
    }
   
    void removeEdge(int from, int to) override {
        for (int i = 0; i < num_of_edges; i++) {
            if ((get<0>(list_of_edges[i]) == from) && (get<1>(list_of_edges[i]) == to)) {
                list_of_edges.erase(list_of_edges.begin() + i);
                break;
            }
        }
        num_of_edges--;
    }
   
    int changeEdge(int from, int to, int newWeight) override {
        int weight = 0;
        for (int i = 0; i < num_of_edges; i++) {
            if ((get<0>(list_of_edges[i]) == from) && (get<1>(list_of_edges[i]) == to)) {
                weight = get<2>(list_of_edges[i]);
                break;
            }
        }
        this->removeEdge(from, to);
        this->addEdge(from, to, newWeight);
        return weight;
    }
   
    vector<vector<int>> transformToAdjMatrix() override {
        vector<vector<int>> adj_matrix;
        for (int i = 0; i < vertex_num; i++) {
            vector<int> row;
            for (int j = 0; j < vertex_num; j++)
                row.push_back(0);
            adj_matrix.push_back(row);
        }
       
        for (int i = 0; i < num_of_edges; i++) {
            adj_matrix[get<0>(list_of_edges[i]) - 1][get<1>(list_of_edges[i]) - 1] = get<2>(list_of_edges[i]);
            if (!is_oriented)
                adj_matrix[get<1>(list_of_edges[i]) - 1][get<0>(list_of_edges[i]) - 1] = get<2>(list_of_edges[i]);
            else
                adj_matrix[get<1>(list_of_edges[i]) - 1][get<0>(list_of_edges[i]) - 1] = -get<2>(list_of_edges[i]);
        }
        return adj_matrix;
    }
   
    vector<set<pair<int, int>>> transformToAdjList() override {
        vector<set<pair<int, int>>> adj_list;
        for (int i = 0; i < vertex_num; i++) {
            set<pair<int, int>> vertex;
            adj_list.push_back(vertex);
        }
       
        for (int i = 0; i < num_of_edges; i++) {
            adj_list[get<0>(list_of_edges[i]) - 1].insert(pair<int, int>(get<1>(list_of_edges[i]), get<2>(list_of_edges[i])));
            if (!is_oriented)
                adj_list[get<1>(list_of_edges[i]) - 1].insert(pair<int, int>(get<0>(list_of_edges[i]), get<2>(list_of_edges[i])));
        }
        return adj_list;
    }
   
    vector <tuple<int, int, int>> transformToListOfEdges() override {
        return list_of_edges;
    }
   
    void writeGraph(string fileName) override {
        ofstream file(fileName);
        file << "E " << vertex_num << " " << list_of_edges.size() << endl;
        file << is_oriented << " " << is_weighted << endl;
        for (int i = 0; i < num_of_edges; i++) {
            file << get<0>(list_of_edges[i]) << " " << get<1>(list_of_edges[i]);
            if (is_weighted)
                file << " " << get<2>(list_of_edges[i]);
            file << endl;
        }
        file.close();
    }
   
    tuple<bool, bool, int> GetInfo() override {
        return tuple<bool, bool, int>(is_weighted, is_oriented, vertex_num);
    }
};
 
class Graph {
private:
    RepresType* repres = nullptr;
public:
    Graph() {
       
    }
   
    Graph(int n) {
        repres = new AdjListGraph(n);
    }
   
    void readGraph(string fileName) {
        ifstream file(fileName);
        char repr;
        file >> repr;
        switch (repr)
        {
            case 'C':    repres = new AdjMatrixGraph(); break;
            case 'L':    repres = new AdjListGraph(); break;
            case 'E':    repres = new ListOfEdgesGraph(); break;
        }
        repres->readGraph(file);
        file.close();
    }
   
    void addEdge(int from, int to, int weight = 1) {
        repres->addEdge(from, to, weight);
    }
   
    void removeEdge(int from, int to) {
        repres->removeEdge(from, to);
    }
   
    int changeEdge(int from, int to, int newWeight) {
        return repres->changeEdge(from, to, newWeight);
    }
   
    void transformToAdjMatrix() {
        repres = new AdjMatrixGraph(repres->transformToAdjMatrix(), repres->GetInfo());
    }
   
    void transformToAdjList() {
        repres = new AdjListGraph(repres->transformToAdjList(), repres->GetInfo());
    }
   
    void transformToListOfEdges() {
        repres = new ListOfEdgesGraph(repres->transformToListOfEdges(), repres->GetInfo());
    }
   
    void writeGraph(string fileName) {
        repres->writeGraph(fileName);
    }
   

    int checkBipart(vector<char> &marks) {
        this->transformToAdjList();
        return reinterpret_cast<AdjListGraph*>(repres)->checkBipart();
    }
   
    vector<pair<int, int> > getMaximumMatchingBipart() {
        this->transformToAdjList();
        return reinterpret_cast<AdjListGraph*>(repres)->getMaximumMatchingBipart();
    }
};

int main()
{
	freopen("output.txt", "w", stdout);
	Graph g;
	g.readGraph("input.txt");
	g.transformToListOfEdges();
	std::vector<char> marks(1e5, ' ');
	if (!g.checkBipart(marks))
		std::cout << -1 << std::endl;
	else
	{
		std::vector<std::pair<int, int>>	vec = g.getMaximumMatchingBipart();
		std::cout << vec.size() << std::endl;
		for (auto i : vec)
			std::cout << i.first << " " << i.second << std::endl;
	}
	return 0;
}
