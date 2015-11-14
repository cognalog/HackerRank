package heap;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Comparator;

/**
 * Solution to <a href="https://www.hackerrank.com/challenges/find-median-1">Find Median</a>.
 * <p>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class MaintainMedian {
    public static class BinaryHeap {
        private final int[] heap;
        private final Comparator<Integer> ordering;
        private int bottom = 0;

        // Using int's natural ordering will produce a max-heap
        public BinaryHeap(final int capacity, final Comparator<Integer> ordering) {
            heap = new int[capacity + 1]; // make this a 1-indexed array for simplicity
            this.ordering = ordering;
        }

        public void add(final int value) {
            int idx = ++bottom;
            if (bottom == heap.length) {
                throw new IllegalStateException("Attempted to exceed heap capacity");
            }
            heap[idx] = value;
            // percolate up
            for (; idx > 1;) {
                final Integer parent = heap[idx / 2];
                if (ordering.compare(heap[idx], parent) > 0) {
                    // swap
                    heap[idx / 2] = heap[idx];
                    heap[idx] = parent;
                    idx /= 2;
                } else {
                    // heap resolved
                    break;
                }
            }
        }

        public int extract() {
            if (bottom == 0) {
                throw new IllegalStateException("Called extract() on empty heap");
            }
            int idx = 1;
            final int top = heap[idx];
            heap[idx] = heap[bottom--];
            // percolate down
            for (; idx < bottom;) {
                final int leftIdx = idx * 2;
                if (leftIdx > bottom) {
                    // no children, heap resolved.
                    break;
                }
                final int leftValue = heap[leftIdx];
                final int rightIdx = leftIdx + 1;
                final int currVal = heap[idx];
                // guard against rightIdx
                if ((rightIdx > bottom || ordering.compare(leftValue, heap[rightIdx]) >= 0)) {
                    if (ordering.compare(currVal, leftValue) < 0) {
                        heap[leftIdx] = currVal;
                        heap[idx] = leftValue;
                        idx = leftIdx;
                    } else {
                        // heap resolved
                        break;
                    }
                } else if (ordering.compare(currVal, heap[rightIdx]) < 0) {
                    heap[idx] = heap[rightIdx];
                    heap[rightIdx] = currVal;
                    idx = rightIdx;
                } else {
                    // heap resolved
                    break;
                }
            }
            return top;
        }

        public int peek() {
            if (bottom == 0) {
                throw new IllegalStateException("Called peek() on empty heap");
            }
            return heap[1];
        }

        public int size() {
            return bottom;
        }
    }

    public static void main(String[] args) throws IOException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final int n = Integer.parseInt(reader.readLine());
        final BinaryHeap lower = new BinaryHeap(n / 2 + 1, Integer::compare);
        final BinaryHeap upper = new BinaryHeap(n / 2 + 1, (x1, x2) -> Integer.compare(x2, x1));
        float median = 0;
        for (int i = 0; i < n; i++) {
            // store new number
            final int num = Integer.parseInt(reader.readLine());
            if (num > median) {
                upper.add(num);
            } else {
                lower.add(num);
            }
            // balance heaps
            if (upper.size() == lower.size() + 2) {
                lower.add(upper.extract());
            }
            if (lower.size() == upper.size() + 2) {
                upper.add(lower.extract());
            }
            // choose new median
            if (upper.size() > lower.size()) {
                median = upper.peek();
            } else if (upper.size() < lower.size()) {
                median = lower.peek();
            } else {
                median = (upper.peek() + lower.peek()) / 2F;
            }
            System.out.println(median);
        }
    }
}
