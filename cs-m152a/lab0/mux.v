`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:37:02 09/23/2021 
// Design Name: 
// Module Name:    mux 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module mux(led, sw
	);
	
	input [4:0] sw;
	output reg led;

	reg not_output,
		buf_output,
		xnor_output,
		xor_output,
		or_output,
		nor_output,
		and_output,
		nand_output;
	
	always @* begin
		not_output = ~sw[0];
		buf_output = sw[0];
		xnor_output = sw[1] ~^ sw[0];
		xor_output = sw[1] ^ sw[0];
		or_output = sw[1] | sw[0];
		nor_output = ~(sw[1] | sw[0]);
		and_output = sw[1] & sw[0];
		nand_output = ~(sw[1] & sw[0]);
		
		case (sw[4:2])
			0:   led = not_output;
			1:   led = buf_output;
			2:   led = xnor_output;
			3:   led = xor_output;
			4:   led = or_output;
			5:   led = nor_output;
			6:   led = and_output;
			7:   led = nand_output;
		endcase
	end

endmodule
