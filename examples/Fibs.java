class Main {
    public static void main() {
        int[][] fibs = { 
              { 0, fib(0) }
            , { 1, fib(1) }
            , { 1, fib(2) }
            , { 2, fib(3) }
            , { 3, fib(4) }
            , { 5, fib(5) }
        };

        for (int i = 0; i < fibs.length; ++i) {
            assert fibs[i][0] == fibs[i][1];
        }
    }

    public static int fib(int x) {
        if (x == 0) {
            return 0;
        } else if (x == 1) {
            return 1;
        } else {
            return fib(x - 1) + fib(x - 2);
        }
    }
}