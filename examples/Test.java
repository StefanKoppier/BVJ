class Main {
    public static void main() {
        Test obj = new Test();
        int x = obj.getVar().getX();
    }
}

class Test2 {
    public Test2() {

    }

    public int getX() {
        return 0;
    }
}

class Test {
    public Test2 var;

    public Test() {
        
    }

    public Test2 getVar() {
        return var;
    }
}
