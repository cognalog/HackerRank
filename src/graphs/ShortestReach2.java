package graphs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * Solution to <a href="https://www.hackerrank.com/challenges/dijkstrashortreach">Dijkstra: Shortest Reach 2</a>
 * <p/>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class ShortestReach2 {

    private static class Vertex {
        private final int id;
        private boolean visited;

        private Vertex(final int id) {
            this.id = id;
            visited = false;
        }

        public int getId() {
            return id;
        }

        public boolean isVisited() {
            return visited;
        }

        public void markVisited() {
            visited = true;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o)
                return true;
            if (o == null || getClass() != o.getClass())
                return false;

            final Vertex vertex = (Vertex) o;

            if (id != vertex.id)
                return false;
            return visited == vertex.visited;

        }

        @Override
        public int hashCode() {
            int result = id;
            result = 31 * result + (visited ? 1 : 0);
            return result;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("Vertex{");
            sb.append("id=").append(id);
            sb.append(", visited=").append(visited);
            sb.append('}');
            return sb.toString();
        }
    }

    private static class Edge {
        private final Vertex vertex;
        private final int weight;

        private Edge(final Vertex vertex, final int weight) {
            this.vertex = vertex;
            this.weight = weight;
        }

        public Vertex getVertex() {
            return vertex;
        }

        public int getWeight() {
            return weight;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder("Edge{");
            sb.append("vertex=").append(vertex);
            sb.append(", weight=").append(weight);
            sb.append('}');
            return sb.toString();
        }
    }

    public static void main(String[] args) throws IOException {

        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final int testCases = Integer.parseInt(reader.readLine());
        for (int i = 0; i < testCases; i++) {
            final String[] nAndM = reader.readLine().split(" ");
            final int n = Integer.parseInt(nAndM[0]);
            final int m = Integer.parseInt(nAndM[1]);
            final Map<Vertex, List<Edge>> graph = new HashMap<>(n);
            final int[] distances = new int[n + 1];
            Arrays.fill(distances, Integer.MAX_VALUE);

            for (int j = 0; j < m; j++) {
                final String[] edgeDef = reader.readLine().split(" ");
                final int x = Integer.parseInt(edgeDef[0]);
                final int y = Integer.parseInt(edgeDef[1]);
                final int r = Integer.parseInt(edgeDef[2]);

                final Vertex v1 = new Vertex(x);
                final Vertex v2 = new Vertex(y);
                if (!graph.containsKey(v1)) {
                    graph.put(v1, new ArrayList<>());
                }
                if (!graph.containsKey(v2)) {
                    graph.put(v2, new ArrayList<>());
                }
                graph.get(v1).add(new Edge(v2, r));
                graph.get(v2).add(new Edge(v1, r));
            }
            final int start = Integer.parseInt(reader.readLine());
            distances[start] = 0;
            final Queue<Vertex> traversalQueue = new PriorityQueue<>((o1, o2) -> {
                return Integer.compare(distances[o1.getId()], distances[o2.getId()]);
            });
            traversalQueue.add(new Vertex(start));
            while (!traversalQueue.isEmpty()) {
                process(traversalQueue.poll(), graph, distances, traversalQueue);
            }
            printDistances(start, distances);
        }

        reader.close();
    }

    private static void process(final Vertex currentV, final Map<Vertex, List<Edge>> graph, final int[] distances,
            final Queue<Vertex> nextToProcess) {
        if (graph.containsKey(currentV)) {
            final List<Edge> edges = graph.get(currentV);
            edges.stream().forEach(e -> {
                final int distanceToNeighbor = e.getWeight() + distances[currentV.getId()];
                final Vertex neighbor = e.getVertex();
                if (distances[neighbor.getId()] > distanceToNeighbor) {
                    distances[neighbor.getId()] = distanceToNeighbor;
                }
                if (!neighbor.isVisited()) {
                    nextToProcess.add(neighbor);
                }
            });
        }
        currentV.markVisited();
    }

    private static void printDistances(final int start, final int[] distances) {
        final StringBuilder result = new StringBuilder();
        for (int i = 1; i < distances.length; i++) {
            if (i == start) {
                continue;
            }
            result.append(distances[i] == Integer.MAX_VALUE ? -1 : distances[i]);
            result.append(i < distances.length - 1 ? " " : "");
        }
        System.out.println(result.toString().trim());
    }
}
