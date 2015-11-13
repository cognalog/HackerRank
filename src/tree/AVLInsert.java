package tree;

/**
 * Solution to <a href="https://www.hackerrank.com/challenges/self-balancing-tree">Self Balancing Tree</a>
 * <p>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class AVLInsert {

    private static class Node {
        private int val;
        private int ht;
        private Node left;
        private Node right;
    }

    static Node insert(Node root, int val) {
        if (root == null) {
            final Node n = new Node();
            n.val = val;
            n.ht = 0;
            return n;
        }
        if (val < root.val) {
            root.left = insert(root.left, val);
        } else {
            root.right = insert(root.right, val);
        }
        root = maybeRotate(root);
        return root;
    }

    private static Node maybeRotate(final Node root) {
        final int balanceFactor = getBalanceFactor(root);
        if (balanceFactor == 2) {
            if (getBalanceFactor(root.left) == -1) {
                root.left = rotateLeft(root.left);
            }
            return rotateRight(root);
        }
        if (balanceFactor == -2) {
            if (getBalanceFactor(root.right) == 1) {
                root.right = rotateRight(root.right);
            }
            return rotateLeft(root);
        }
        correctHeight(root);
        return root;
    }

    private static Node rotateRight(final Node root) {
        final Node newRoot = root.left;
        root.left = root.left.right;
        newRoot.right = root;
        correctHeight(root);
        correctHeight(newRoot);
        return newRoot;
    }

    private static Node rotateLeft(final Node root) {
        final Node newRoot = root.right;
        root.right = root.right.left;
        newRoot.left = root;
        correctHeight(root);
        correctHeight(newRoot);
        return newRoot;
    }

    private static int getBalanceFactor(final Node root) {
        final int leftHeight = root.left == null ? -1 : root.left.ht;
        final int rightHeight = root.right == null ? -1 : root.right.ht;
        return leftHeight - rightHeight;
    }

    private static void correctHeight(final Node root) {
        root.ht = 1 + Math.max(root.left == null ? -1 : root.left.ht, root.right == null ? -1 : root.right.ht);
    }
}
