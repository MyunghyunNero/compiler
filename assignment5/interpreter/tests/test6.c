
int fibo(int n)
{
	if (n < 2) return n;
	if (n == 1)
		return 1;
	return fibo(n - 1) + fibo(n - 2);
}

int main()
{
	int i = 0;
	fibo(3);

	return 0;
}