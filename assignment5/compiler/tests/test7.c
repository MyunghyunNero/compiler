struct s {
    struct s *a;
    int b; 
} lee[10];
int main() {
    int i = 0;
    int a = 10, b = 5, c = 1, d = 2;
    printf("%d\n",lee[1].a->b++);
    printf("%d\n",sizeof(float*[10]));
    printf("%d\n",(int)(a + 3.14));
    return 0;
}
