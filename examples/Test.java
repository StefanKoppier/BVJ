class Main {
    public static void main() {
        A obj = null;
        int x = obj.getB().getC().getInt();
    }
}

class A {
    private B value;

    public B getB() {
        return value;
    }
}

class B {
    private C value;

    public C getC() {
        return value;
    }
}

class C {
    public int getInt() {
        return 0;
    }
}