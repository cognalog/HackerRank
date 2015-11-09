package tree;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

/**
 * Solution for <a href="https://www.hackerrank.com/challenges/no-prefix-set">No Prefix Set</a>
 * <p>
 * 2015
 *
 * @author Tyrone Hinderson (╯°□°）╯︵ ┻━┻
 */
public class NoPrefixSet {

    private static class Vertex {
        private boolean wholeWord = false;
        private final Map<Character, Vertex> children = new HashMap<>();

        public boolean isWholeWord() {
            return wholeWord;
        }

        public boolean hasChildren() {
            return !children.isEmpty();
        }

        public void markWholeWord() {
            wholeWord = true;
        }

        public Vertex getChild(final char c) {
            return children.get(c);
        }

        public boolean hasChild(final char c) {
            return children.containsKey(c);
        }

        public void addChild(final char c) {
            children.put(c, new Vertex());
        }
    }

    public static void main(String[] args) throws IOException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final int n = Integer.parseInt(reader.readLine());
        final Vertex trie = new Vertex();
        boolean bad = false;
        for (int i = 0; i < n; i++) {
            final String str = reader.readLine();
            if (addWord(trie, str)) {
                System.out.println("BAD SET");
                System.out.println(str);
                bad = true;
                break;
            }
        }
        if (!bad) {
            System.out.println("GOOD SET");
        }
    }

    /**
     * Add a word to a trie.
     *
     * @param trie
     *            the trie
     * @param s
     *            the word
     * @return {@code true} if this word is a prefix of an already-trie'd word (or vice versa), {@code false} otherwise.
     */
    private static boolean addWord(final Vertex trie, final String s) {
        if (trie.isWholeWord()) {
            return true;
        }
        if (s.isEmpty()) { // we're done adding the word. Mark this node and ensure it's a leaf.
            trie.markWholeWord();
            return trie.hasChildren(); // is this word the prefix of another?
        }

        final char firstChar = s.charAt(0);
        if (!trie.hasChild(firstChar)) {
            trie.addChild(firstChar);
        }
        return addWord(trie.getChild(firstChar), s.substring(1));
    }
}
