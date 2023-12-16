int main() {
	int x = 0;
	int i;
	while(1) {
		x = x+1; if(x > 5) break;
	}
	for(i = 0; i < 5; i++) {
		++x;
	}
	return 0;
}