#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <stdio.h>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

typedef priority_queue<tuple<int, int, int>, vector<tuple<int, int, int>>, greater<tuple<int, int, int>>> priority_q;

class Graph {
  enum GraphType { TAM, TAL, LOE };
  GraphType type;
  bool oriented, weighted;
  int N, M;
  char graphType;
  vector<vector<int>> adjMatrix;
  vector<set<int>> adjListNotWght;
  vector<map<int, int>> adjListWght;
  vector<map<int, int>> adjList;
  map<pair<int, int>, int> listOfEdges;
  set<pair<int, int>> listOfEdgesNotWght;
  map<pair<int, int>, int> listOfEdgesWght;

public:
  Graph() {}
  Graph(int n, bool weighted = true, bool orient = false) {
    this->weighted = weighted;
    oriented = orient;
    M = 0;
    N = n;
    type = GraphType::LOE;
  }
  void readGraph(std::string fileName);
  void addEdge(int from, int to, int weight);
  void removeEdge(int from, int to);
  int changeEdge(int from, int to, int newWeight);
  void transformToAdjMatrix();
  void transformToAdjList();
  void transformToListOfEdges();
  void writeGraph(std::string fileName) { writeG(fileName); }

private:
  void readGraph(ifstream &fin);
  void addEdgeAdjList(int from, int to, int weight);
  void addEdgeAdjMatrix(int from, int to, int weight);
  void addEdgeListOfEdges(int from, int to, int weight);
  void removeEdgeAdjList(int from, int to);
  void removeEdgeAdjMatrix(int from, int to);
  void removeEdgeListOfEdges(int from, int to);
  int getWeightAdjList(int from, int to) const {
    return adjListWght[from - 1].at(to);
  }
  int getWeightAdjMatrix(int from, int to) const {
    return adjMatrix[from - 1][to - 1];
  }
  int getWeightListOfEdges(int from, int to) const;
  void transformMatrixToListOfEdges();
  void transformMatrixToAdjList();
  void transformAdjListToListOfEdges();
  void transformAdjListToMatrix();
  void transformListOfEdgesToAdjList();
  void transformlistOfEdgesToMatrix();
  void writeG(string fileName);
  priority_q getweightedEdgesQueue();
  void swap(int &a, int &b) const {
    int t = b;
    b = a;
    a = t;
  }
  int getRoot(vector<bool> MSTcontains, int finalRoot);
  bool algorithmOfKuhn(int u, map<int, set<int>> &part1, map<int, int> &part2, map<int, bool> &inDepthSearch) const;

public:
  int checkBipart(vector<char> &marks);
  vector<pair<int, int>> getMaximumMatchingBipart();
};

void Graph::readGraph(ifstream &fin) {
  int i, j, from, to, weight, flag, tmp;
  fin >> N;
  if (type == GraphType::LOE)
    fin >> M;
  fin >> flag;
  oriented = (bool)flag;
  fin >> flag;
  weighted = (bool)flag;
  if (type == GraphType::TAM) {
    adjMatrix.resize(N);
    for (i = 0; i < N; i++) {
      adjMatrix.push_back(vector<int>());
      for (j = 0; j < N; j++) {
        fin >> tmp;
        adjMatrix[i].push_back(tmp);
        M++;
      }
    }
  } else if (type == GraphType::TAL) {
    adjListWght.resize(N);
    for (i = 0; i < N; i++) {
      string line;
      getline(fin, line);
      istringstream row(line);
      int vertex, weight;
      while (row >> vertex && row >> weight)
        adjListWght[i].insert(make_pair(vertex, weight));
    }
  } else {
    for (i = 0; i < M; i++) {
      fin >> from >> to >> weight;
      if (from > to)
        swap(from, to);
      listOfEdgesWght.insert(make_pair(make_pair(from, to), weight));
    }
  }
}

void Graph::addEdgeAdjList(int from, int to, int weight) {
  adjListWght[from - 1].insert(make_pair(to, weight));
  adjListWght[to - 1].insert(make_pair(from, weight));
}

void Graph::addEdgeAdjMatrix(int from, int to, int weight) {
  adjMatrix[from - 1][to - 1] = weighted ? weight : 1;
  adjMatrix[to - 1][from - 1] = weighted ? weight : 1;
}

void Graph::addEdgeListOfEdges(int from, int to, int weight) {
  if (from > to)
    swap(from, to);
  listOfEdgesWght.insert(make_pair(make_pair(from, to), weight));
}

void Graph::removeEdgeAdjList(int from, int to) {
  adjListWght[from - 1].erase(to);
  adjListWght[to - 1].erase(from);
}

void Graph::removeEdgeAdjMatrix(int from, int to) {
  adjMatrix[from - 1][to - 1] = 0;
  if (!oriented)
    adjMatrix[to - 1][from - 1] = 0;
}

void Graph::removeEdgeListOfEdges(int from, int to) {
  if (from > to)
    swap(from, to);
  if (weighted)
    listOfEdgesWght.erase(make_pair(from, to));
  else
    listOfEdgesNotWght.erase(make_pair(from, to));
}

int Graph::getWeightListOfEdges(int from, int to) const {
  if (from > to)
    swap(from, to);
  return listOfEdgesWght.at(make_pair(from, to));
}

void Graph::transformMatrixToListOfEdges() {
  int from, to;
  for (from = 0; from < N; from++) {
    int startIndex = oriented ? 0 : from;
    for (to = startIndex; to < N; to++)
      if (adjMatrix[from][to] != 0)
        addEdgeListOfEdges(from + 1, to + 1, adjMatrix[from][to]);
  }
  adjMatrix.clear();
}

void Graph::transformMatrixToAdjList() {
  int from, to;
  if (weighted)
    adjListWght.resize(N);
  else
    adjListNotWght.resize(N);
  for (from = 0; from < N; from++)
    for (to = 0; to < N; to++)
      if (adjMatrix[from][to] != 0)
        addEdgeAdjList(from + 1, to + 1, adjMatrix[from][to]);
  adjMatrix.clear();
}

void Graph::transformAdjListToListOfEdges() {
  for (int from = 0; from < N; from++)
    for (auto pair : adjListWght[from])
      addEdgeListOfEdges(from + 1, pair.first, pair.second);
  adjListWght.clear();
}

void Graph::transformAdjListToMatrix() {
  adjMatrix.resize(N);
  for (int from = 0; from < N; from++) {
    adjMatrix[from].resize(N);
    for (auto pair : adjListWght[from])
      adjMatrix[from][pair.first - 1] = pair.second;
  }
  adjListWght.clear();
}

void Graph::transformListOfEdgesToAdjList() {
  adjListWght.resize(N);
  for (auto pair : listOfEdgesWght)
    addEdgeAdjList(pair.first.first, pair.first.second, pair.second);
  listOfEdgesWght.clear();
}

void Graph::transformlistOfEdgesToMatrix() {
  int i, from, to, weight;
  adjMatrix.resize(N);
  for (i = 0; i < N; i++)
    adjMatrix[i].resize(N);
  for (auto pair : listOfEdgesWght) {
    from = pair.first.first;
    to = pair.first.second;
    weight = pair.second;
    adjMatrix[from - 1][to - 1] = weight;
    adjMatrix[to - 1][from - 1] = weight;
  }
  listOfEdgesWght.clear();
}

void Graph::writeG(string fileName) {
  int i, j;
  ofstream fout;
  fout.open(fileName);
  if (type == GraphType::TAM)
    fout << "C ";
  else if (type == GraphType::TAL)
    fout << "L ";
  else
    fout << "E ";
  fout << N;
  if (type == GraphType::LOE)
    fout << ' ' << M;
  fout << endl << (int)oriented << ' ' << (int)weighted << endl;
  if (type == GraphType::TAM) {
    for (i = 0; i < N; i++) {
      for (j = 0; j < N - 1; j++)
        fout << adjMatrix[i][j] << ' ';
      fout << adjMatrix[i][N - 1] << endl;
    }
  } else if (type == GraphType::TAL) {
    for (i = 0; i < N; i++) {
      auto it = adjListWght[i].begin();
      fout << it->first << ' ' << it->second;
      it++;
      while (it != adjListWght[i].end()) {
        fout << ' ' << it->first << ' ' << it->second;
        it++;
      }
      fout << endl;
    }
  } else
    for (auto pair : listOfEdgesWght)
      fout << pair.first.first << ' ' << pair.first.second << ' ' << pair.second << endl;
  fout.close();
}

void Graph::readGraph(std::string fileName) {
  ifstream fin;
  fin.open(fileName);
  char typeChar;
  fin >> typeChar;
  if (typeChar == 'C')
    type = GraphType::TAM;
  else if (typeChar == 'L')
    type = GraphType::TAL;
  else
    type = GraphType::LOE;
  readGraph(fin);
  fin.close();
}

void Graph::addEdge(int from, int to, int weight) {
  if (type == GraphType::TAM)
    addEdgeAdjMatrix(from, to, weight);
  else if (type == GraphType::TAL)
    addEdgeAdjList(from, to, weight);
  else
    addEdgeListOfEdges(from, to, weight);
  M++;
}

void Graph::removeEdge(int from, int to) {
  if (type == GraphType::TAM)
    removeEdgeAdjMatrix(from, to);
  else if (type == GraphType::TAL)
    removeEdgeAdjList(from, to);
  else
    removeEdgeListOfEdges(from, to);
  M--;
}

int Graph::changeEdge(int from, int to, int newWeight) {
  int oldWeight;
  if (type == GraphType::TAM) {
    oldWeight = getWeightAdjMatrix(from, to);
    addEdgeAdjMatrix(from, to, newWeight);
  } else if (type == GraphType::TAL) {
    oldWeight = getWeightAdjList(from, to);
    removeEdgeAdjList(from, to);
    addEdgeAdjList(from, to, newWeight);
  } else {
    oldWeight = getWeightListOfEdges(from, to);
    removeEdgeListOfEdges(from, to);
    addEdgeListOfEdges(from, to, newWeight);
  }
  return oldWeight;
}

void Graph::transformToAdjMatrix() {
  if (type == GraphType::TAL)
    transformAdjListToMatrix();
  else if (type == GraphType::LOE)
    transformlistOfEdgesToMatrix();
  type = GraphType::TAM;
}

void Graph::transformToAdjList() {
  if (type == GraphType::TAM)
    transformMatrixToAdjList();
  else if (type == GraphType::LOE)
    transformListOfEdgesToAdjList();
  type = GraphType::TAL;
}

void Graph::transformToListOfEdges() {
  if (type == GraphType::TAM)
    transformMatrixToListOfEdges();
  else if (type == GraphType::TAL)
    transformAdjListToListOfEdges();
  type = GraphType::LOE;
}

priority_q Graph::getweightedEdgesQueue() {
  priority_q result;
  int i, j;
  switch (type) {
  case Graph::TAM:
    for (i = 0; i < N - 1; i++)
      for (j = i + 1; j < N; j++)
        result.push(make_tuple(adjMatrix[i][j], i + 1, j + 1));
    break;
  default:
    transformToListOfEdges();
    for (auto edge : listOfEdgesWght)
      result.push(make_tuple(edge.second, edge.first.first, edge.first.second));
    break;
  }
  return result;
}

int Graph::getRoot(vector<bool> MSTcontains, int finalRoot) {
  for (int i = finalRoot + 1; i < MSTcontains.size(); i++)
    if (!(MSTcontains[i]))
      return i;
  return -1;
}

bool Graph::algorithmOfKuhn(int u, map<int, set<int>> &part1, map<int, int> &part2, map<int, bool> &inDepthSearch) const {
  if (inDepthSearch[u])
    return false;
  inDepthSearch[u] = true;
  for (auto v = part1[u].begin(); v != part1[u].end(); v++)
    if (part2[*v] == -1 ||
        algorithmOfKuhn(part2[*v], part1, part2, inDepthSearch)) {
      part2[*v] = u;
      return true;
    }
  return false;
}

vector<pair<int, int>> Graph::getMaximumMatchingBipart() {
  vector<char> marks;
  vector<pair<int, int>> answer;
  int minPart = checkBipart(marks);
  map<int, set<int>> part1;
  map<int, int> part2;
  map<int, bool> inHeuristics, inDepthSearch;
  int aCounter = 0, bCounter = 0;
  vector<set<int>> tempGraph(N);
  if (minPart == 0)
    return answer;
  if (graphType == 'C') {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
        if (adjMatrix[i][j] > 0) {
          tempGraph[i].insert(j);
          tempGraph[j].insert(i);
        }
  } else {
    if (graphType == 'L') {
      for (int i = 0; i < N; i++)
        for (auto j = adjList[i].begin(); j != adjList[i].end(); j++) {
          tempGraph[i].insert(j->first - 1);
          tempGraph[j->first - 1].insert(i);
        }
    } else
      for (auto i = listOfEdges.begin(); i != listOfEdges.end(); i++) {
        tempGraph[i->first.first - 1].insert(i->first.second - 1);
        tempGraph[i->first.second - 1].insert(i->first.first - 1);
      }
  }
  for (int i = 0; i < N; i++) {
    if (marks[i] == 'A')
      aCounter++;
    else
      bCounter++;
  }
  if (aCounter < bCounter)
    for (int i = 0; i < N; i++) {
      if (marks[i] == 'A') {
        part1[i] = tempGraph[i];
        inHeuristics[i] = false;
        inDepthSearch[i] = false;
      } else
        part2[i] = -1;
    }
  else
    for (int i = 0; i < N; i++) {
      if (marks[i] == 'B') {
        part1[i] = tempGraph[i];
        inHeuristics[i] = false;
        inDepthSearch[i] = false;
      } else
        part2[i] = -1;
    }
  for (auto u = part1.begin(); u != part1.end(); u++)
    for (auto v = u->second.begin(); v != u->second.end(); v++)
      if (part2[*v] == -1) {
        part2[*v] = u->first;
        inHeuristics[u->first] = true;
        break;
      }
  for (auto u = part1.begin(); u != part1.end(); u++) {
    if (inDepthSearch[u->first])
      continue;
    for (auto i = inDepthSearch.begin(); i != inDepthSearch.end(); i++)
      i->second = false;
    algorithmOfKuhn(u->first, part1, part2, inDepthSearch);
  }

  for (auto edge = part2.begin(); edge != part2.end(); edge++)
    if (edge->second != -1)
      answer.push_back(make_pair(edge->first + 1, edge->second + 1));
  return answer;
}

int Graph::checkBipart(vector<char> &marks) {
  if (N >= 2) {
    marks = vector<char>(N, 'O');
    vector<set<int>> tempGraph(N);
    int u;
    char uMark, vMark = ' ';
    u = 0;
    queue<int> Q;
    if (graphType == 'C') {
      for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
          if (adjMatrix[i][j] > 0) {
            tempGraph[i].insert(j);
            tempGraph[j].insert(i);
          }
    } else {
      if (graphType == 'L') {
        for (int i = 0; i < N; i++)
          for (auto j = adjList[i].begin(); j != adjList[i].end(); j++) {
            tempGraph[i].insert(j->first - 1);
            tempGraph[j->first - 1].insert(i);
          }
      } else
        for (auto i = listOfEdges.begin(); i != listOfEdges.end(); i++) {
          tempGraph[i->first.first - 1].insert(i->first.second - 1);
          tempGraph[i->first.second - 1].insert(i->first.first - 1);
        }
    }
    for (int i = 0; i < N; i++) {
      if (marks[i] == 'O') {
        marks[i] = 'A';
        Q.push(i);
      }
      while (!(Q.empty())) {
        u = Q.front();
        Q.pop();
        uMark = marks[u];
        if (uMark == 'A')
          vMark = 'B';
        else
          vMark = 'A';
        for (auto v = tempGraph[u].begin(); v != tempGraph[u].end(); v++) {
          if (marks[*v] == 'O') {
            marks[*v] = vMark;
            Q.push(*v);
          } else if (marks[*v] == uMark)
            return 0;
        }
      }
    }
    return 1;
  } else
    return 0;
}

int main() {
  freopen("output.txt", "w", stdout);
  Graph g;
  g.readGraph("input.txt");
  g.transformToListOfEdges();
  vector<char> marks(1e5, ' ');
  if (!g.checkBipart(marks))
    cout << -1 << endl;
  else {
    vector<pair<int, int>> vec = g.getMaximumMatchingBipart();
    cout << vec.size() << endl;
    for (auto i : vec)
      cout << i.first << " " << i.second << endl;
  }
  return 0;
}
