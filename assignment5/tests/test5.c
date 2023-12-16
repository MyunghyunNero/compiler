int main() { 
	int a;
	int *b =&a; 
	int **p = &b; 
	*b;
	**p;
	return 0;
}