class Main {
    public static void main() {
        Test obj = new Test();

        for (int i = 0; i < 3; i++) {
            obj.setX(i);
            assert obj.getX() == i;
        }
    }
}

class Test {
    private int x;

    public Test() {
        
    }
    
    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }
}
