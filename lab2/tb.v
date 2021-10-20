`timescale 1ns / 1ps

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   11:56:16 10/14/2021
// Design Name:   top
// Module Name:   E:/CS152A/lab2/tb.v
// Project Name:  lab2
// Target Device:  
// Tool versions:  
// Description: 
//
// Verilog Test Fixture created by ISE for module: top
//
// Dependencies:
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
////////////////////////////////////////////////////////////////////////////////

module tb;

	// Outputs
	reg [11:0] D;
	wire S;
	wire [2:0] E;
	wire [3:0] F;
	
	reg answer_S;
	reg [2:0] answer_E;
	reg [3:0] answer_F;
	
	wire correct;
	
	assign correct = S == answer_S && E == answer_E && F == answer_F;

	// Instantiate the Unit Under Test (UUT)
	top uut (
		.D(D),
		.S(S),
		.E(E),
		.F(F)
	);

	initial begin
		D <= -2048;
		answer_S <= 1;
		answer_E <= 7;
		answer_F <= 15;
		#5;
		
		D <= -2047;
		answer_S <= 1;
		answer_E <= 7;
		answer_F <= 15;
		#5;
		
		D <= 0;
		answer_S <= 0;
		answer_E <= 0;
		answer_F <= 0;
		#5;
		
		D <= 1;
		answer_S <= 0;
		answer_E <= 0;
		answer_F <= 1;
		#5;
		
		D <= -1;
		answer_S <= 1;
		answer_E <= 0;
		answer_F <= 1;
		#5;
		
		D <= 'b000110011101;
		answer_S <= 0;
		answer_E <= 5;
		answer_F <= 'b1101;
		#5;
		
		D <= 'b000111111101;
		answer_S <= 0;
		answer_E <= 6;
		answer_F <= 'b1000;
		#5;
		
		D <= 'b001100111010;
		answer_S <= 0;
		answer_E <= 6;
		answer_F <= 'b1101;
		#5;
		
		D <= 'b001111111010;
		answer_S <= 0;
		answer_E <= 7;
		answer_F <= 'b1000;
		#5;
		
		D <= 'b011000110101;
		answer_S <= 0;
		answer_E <= 7;
		answer_F <= 'b1100;
		#5;
		
		D <= 2047;
		answer_S <= 0;
		answer_E <= 7;
		answer_F <= 15;
		#5;
		
		D <= 'b000010010111;
		answer_S <= 0;
		answer_E <= 4;
		answer_F <= 'b1001;
		#5;
		
		D <= 'b000010011111;
		answer_S <= 0;
		answer_E <= 4;
		answer_F <= 'b1010;
		#5;
		
		D <= 'b000001100100;
		answer_S <= 0;
		answer_E <= 3;
		answer_F <= 'b1101;
		#5;
		
		D <= 'b000001100000;
		answer_S <= 0;
		answer_E <= 3;
		answer_F <= 'b1100;
		#5;
		
		D <= 'b000000101101;
		answer_S <= 0;
		answer_E <= 2;
		answer_F <= 'b1011;
		#5;
		
		D <= 'b000000101111;
		answer_S <= 0;
		answer_E <= 2;
		answer_F <= 'b1100;
		#5;
		
		D <= 'b000000010110;
		answer_S <= 0;
		answer_E <= 1;
		answer_F <= 'b1011;
		#5;
		
		D <= 'b000000010111;
		answer_S <= 0;
		answer_E <= 1;
		answer_F <= 'b1100;
		#5;
		
		D <= 13;
		answer_S <= 0;
		answer_E <= 0;
		answer_F <= 13;
		#5;

		D <= 'b100111001010;
		//    -011000110110
		answer_S <= 1;
		answer_E <= 7;
		answer_F <= 'b1100;
		#5;

		$finish;
	end
      
endmodule

