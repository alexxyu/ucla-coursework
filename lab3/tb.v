`timescale 1us / 1us

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   15:54:19 10/26/2021
// Design Name:   top
// Module Name:   E:/CS152A/lab3/tb.v
// Project Name:  lab3
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

	// Inputs
	reg clk = 0;
	
	reg [1:0] sw = 0;
	reg btnl = 0, btnr = 0;

	// Outputs
	wire [6:0] seg;
	wire [3:0] an;

	// Instantiate the Unit Under Test (UUT)
	top uut (
		.clk	(clk),
		.sw	(sw),
		.btnl	(btnl),
		.btnr	(btnr),
		.seg	(seg), 
		.an	(an)
	);

	initial begin
		// Initialize Inputs
		clk = 0;

		// Wait 100 ns for global reset to finish
		#100;
        
		// Pause and unpause
		btnl = 1;
		#10;
		btnl = 0;
		#20;
		btnl = 1;
		#20;
		btnl = 0;
		#100;
		btnl = 1;
		#1000000;
		btnl = 0;
		#1000000;
		btnl = 1;
		#1000000;
		btnl = 0;
		#1000000;
		
		// Reset
		btnr = 1;
		#10;
		btnr = 0;
		#20;
		btnr = 1;
		#20;
		btnr = 0;
		#100;
		btnr = 1;
		#1000000;
		btnr = 0;
		#1000000;
		
		// Adjust seconds
		sw[0] = 1;
		sw[1] = 1;
		#1000000;
		
		// Adjust minutes
		sw[1] = 0;
		#1000000;
		
		// Back to counting
		sw[0] = 0;
		#1000000;
		
		#10000000;
		$finish;
	end
	
	always begin
		#1 clk = ~clk;
	end
      
endmodule

