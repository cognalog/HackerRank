package heap;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.PriorityQueue;

/**
 * Solution for <a href="https://www.hackerrank.com/challenges/minimum-average-waiting-time">Minimum Average Waiting
 * Time</a>
 * <p>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class MinAvgWaitTime {

    public static class Customer {
        private int arrivalTime;
        private int pizzaTime;

        public Customer(final String customerString) {
            final String[] customer = customerString.split(" ");
            arrivalTime = Integer.parseInt(customer[0]);
            pizzaTime = Integer.parseInt(customer[1]);
        }
    }

    public static void main(String[] args) throws IOException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final int n = Integer.parseInt(reader.readLine());
        final PriorityQueue<Customer> arrivals = new PriorityQueue<>(n, (c1, c2) -> Integer.compare(c1.arrivalTime,
                c2.arrivalTime));
        final PriorityQueue<Customer> pizzas = new PriorityQueue<>(n, (c1, c2) -> Integer.compare(c1.pizzaTime,
                c2.pizzaTime));
        long waitSum = 0;

        // bring in the customers
        for (int i = 0; i < n; i++) {
            final Customer cust = new Customer(reader.readLine());
            arrivals.add(cust);
        }
        reader.close();
        for (long t = 0; !arrivals.isEmpty() || !pizzas.isEmpty();) {
            // queue up the customers who've arrived by now
            for (Customer c = arrivals.peek(); c != null && c.arrivalTime <= t; c = arrivals.peek()) {
                pizzas.add(arrivals.poll());
            }
            // cook a customer's pizza and record his/her total wait
            final Customer currentCust = pizzas.poll();
            if (currentCust != null) {
                t += currentCust.pizzaTime;
                waitSum += t - currentCust.arrivalTime;
            } else {
                t++;
            }
        }
        System.out.println(waitSum / n);
    }
}
