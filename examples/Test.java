class Main {
    public static void main() {
        Test var = new Test(10);
        
        for (int i = 0; i < 5; i++) {
            var.setX(i);
            assert var.getX() == i : "var.getX() != i";
        }
    }
}

class Test2 {
    public int getX() {
        return 0;
    }
}

class Test {
    public int x = 0;
    
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
