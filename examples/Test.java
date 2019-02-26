class Main {
    public static void main() {
        Test x = new Test(10);

        assert x.getX() == 10;
    }

    public static int id(int x) {
        return x;
    }
}

class Test {
    private int x;

    public Test(int x) {
        this.x = x;
    }
    
    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }
}
