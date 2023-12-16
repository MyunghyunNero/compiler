typedef struct node {
    char *name;
} tnode;

void* func(tnode *n, char *name) {

    n->name = name;

    return n;
}

int main() {
    tnode n;
    tnode *ptr = func(&n, "name1");
    printf("%s\n", ptr->name);
    return 0;
}