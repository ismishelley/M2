int main(){
	float a;
	int b;

	a = 9.3;

	if (a > 5.1){
		printStr("If statement");
	}
	else{
		printStr("Else statement");
	}

	while (a > 8.0){
		a = a - 2.0;
		printStr("While loop");
	}

	for (b = 0; b < 2; ++b){
		printStr("For loop");
	}

	return 0;
}