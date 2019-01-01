class Node
{
    private int value;

    public Node(int value)
    {
        this.value = value;
    }

    public int GetValue()
    {
        return this.value;
    }

    public int SetValue(int value)
    {
        this.value = value;
    }
}

/*

struct class_node
{
    int value;
    
} class_node_t;

class_node_t* constructor_Node_1(int value)
{
    // Initialization code, should be there for every constructor. 
    class_node_t* this = malloc(sizeof(class_node_t));
    
    // Body of the original constructor, translated to C.
    this->value = value;
    
    // Return code, should be there for every constructor.
    return this;
}

int method_Node_GetValue_1(const Node* this)
{
    return this->value;
}

void method_Node_SetValue_1(const Node* this, int value)
{

}

*/