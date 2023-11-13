struct s {
	struct s *a;
	int b; 
} lee[10];
void fun(int, int);
int main() {
	int k[5] = {0};
	int i = 0;
	int a = 10, b = 5, c = 1, d = 2;
	k[i+3];
	lee[1].a->b++;
	&k[2];
	sizeof(float*[10]);
	(int)(a + 3.14);
	fun(2, i + 10);
	a+b*c||d;
	return 0;
}