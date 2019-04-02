import java.io.*;


class three_x_plus_1
{
	public static int abs(int x){
		if (x < 0) return -1*x; else return x;
	}

	public static int exp(int x, int y){
		int result = 1;

		while (y-- != 0) {
			result *= x;
		}
		return result;
	}

    public static void main( String[] args) {
        int sum  = 0;
		  int sum2 = 0;
        
		  {
				int x;
				
				for (int i = 1; i < 32; i++) {
					x = i;
					while (x != 1) {
						for (int j = 1; j < 3; j++){
							if (x % 2 == 0) x = x / 2;
							else x = 3*x + 1;
						}
						sum = sum + 3;
					}
				}
		}

        
		{
			int sum1 = 0;
			int x;
			
			for (int i = 15; i > 1; --i) {
				x = i;
				while (x != 1) {
					for (int j = 1; j < 3; ++j){
						if (x % 2 == 0) x = x / 2;
						else x = 3*x + 1;
					}
					sum1 = sum1 + 3;
				}
			}
			System.out.println("\nsum1 = " + sum1);
		}
		
		{
			int x;
			
			for (int i = -24; i < -1; ++i) {
				x = i;
				while (x != 1) {
					for (int j = 1; j < 3; ++j){
						if (x % 2 == 0) x = abs(x / 2);
						else x = (abs(3*x) + 1);
					}
					sum2 = ((sum2 + 3));
				}
			}
			System.out.println("\nsum2 = " + sum2);
		}
		System.out.println("\nsum = " + sum);


		if (sum2 == 423 && !(sum != 888))
   		if (sum2 + 465 >= sum && sum <= sum2 + 465) 
				if (exp(2,0) < 2 && exp(2,1) > 2 || exp(2,3) < 8) System.out.println("\nBranch = " + 1);
				else System.out.println("\nBranch = " + 2);
			else System.out.println("\nBranch = " + 3);
		else System.out.println("\nBranch =" + 4);
	}
}
