package graphs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;

/**
 * Solution for <a href="https://www.hackerrank.com/challenges/bfsshortreach">Breadth First Search: Shortest Reach</a>.
 * Lots in common with {@link ShortestReach2}, since this is basically a simplified version.
 * <p/>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class ShortestReach {

    private static final int EDGE_WEIGHT = 6;

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

    public static void main(String[] args) throws IOException {

        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final int testCases = Integer.parseInt(reader.readLine());
        for (int i = 0; i < testCases; i++) {
            final String[] nAndM = reader.readLine().split(" ");
            final int n = Integer.parseInt(nAndM[0]);
            final int m = Integer.parseInt(nAndM[1]);
            final Map<Vertex, List<Vertex>> graph = new HashMap<>(n);
            final int[] distances = new int[n + 1];
            Arrays.fill(distances, Integer.MAX_VALUE);

            for (int j = 0; j < m; j++) {
                final String[] edgeDef = reader.readLine().split(" ");
                final int x = Integer.parseInt(edgeDef[0]);
                final int y = Integer.parseInt(edgeDef[1]);

                final Vertex v1 = new Vertex(x);
                final Vertex v2 = new Vertex(y);
                if (!graph.containsKey(v1)) {
                    graph.put(v1, new ArrayList<>());
                }
                if (!graph.containsKey(v2)) {
                    graph.put(v2, new ArrayList<>());
                }
                graph.get(v1).add(v2);
                graph.get(v2).add(v1);
            }
            final int start = Integer.parseInt(reader.readLine());
            distances[start] = 0;
            final Queue<Vertex> traversalQueue = new ArrayDeque<>();
            traversalQueue.add(new Vertex(start));
            while (!traversalQueue.isEmpty()) {
                process(traversalQueue.poll(), graph, distances, traversalQueue);
            }
            printDistances(start, distances);
        }

        reader.close();
    }

    private static void process(final Vertex currentV, final Map<Vertex, List<Vertex>> graph, final int[] distances,
            final Queue<Vertex> traversalQueue) {
        if (graph.containsKey(currentV)) {
            final List<Vertex> neighbors = graph.get(currentV);
            neighbors.stream() //
                    .filter(v -> distances[v.getId()] == Integer.MAX_VALUE) //
                    .forEach(v -> {
                        distances[v.getId()] = EDGE_WEIGHT + distances[currentV.getId()];
                        traversalQueue.add(v);
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
