class Main {
    public static void main() {
        Container[] array = new Container[2];
        array[0] = new Container(0);
        array[1] = new Container(1);
        
        assert array[1].x == 1;
    }
}

class Container {
    public int x;

    public Container(int x) {
        this.x = x;
    }
}