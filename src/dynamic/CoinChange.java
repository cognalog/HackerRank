package dynamic;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Solution for <a href="https://www.hackerrank.com/challenges/coin-change">The Coin Change Problem</a>
 * <p>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class CoinChange {

    public static void main(String[] args) throws IOException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final String[] nAndM = reader.readLine().split(" ");
        final int n = Integer.parseInt(nAndM[0]);
        final int m = Integer.parseInt(nAndM[1]);
        final List<Integer> coins = m > 0 ? Arrays.asList(reader.readLine().split(" ")).stream()
                .map(Integer::parseInt).collect(Collectors.toList()) : Collections.emptyList();
        final long[][] solutions = new long[n + 1][coins.size()];
        for (final long[] row : solutions) {
            Arrays.fill(row, -1L);
        }
        final long result = soln(n, m - 1, coins, solutions);
        System.out.println(result);
    }

    private static long soln(final int n, final int m, final List<Integer> coins, final long[][] solutions) {
        if (n == 0) {
            return 1;
        }
        if (n < 0 || m < 0) {
            return 0;
        }

        if (solutions[n][m] != -1) {
            return solutions[n][m];
        }

        final long solution = soln(n, m - 1, coins, solutions) + soln(n - coins.get(m), m, coins, solutions);
        solutions[n][m] = solution;
        return solution;
    }
}
