class Main {
    public static void main() {
        assert square(2) == (square(2) + id(0)) : "square(2) != square(2)";
    }

    public int id(int x) { 
        int r = 0;
        for (int i = 0; i < x; i++)  {
            r += 1;
        }
        return r;
    }

    public int square(int x) {
        return x * x;
    }
}