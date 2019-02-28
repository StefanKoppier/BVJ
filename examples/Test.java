class Main {
    static Test obj;

    public static void main() {
        assert Main.obj.x == 1;
    }
}

class Test {
    public int x;
    public static int y = 1;

    public Test(int x) {
        this.x = x;
    }

    public int getX() {
        return y;
    }
}