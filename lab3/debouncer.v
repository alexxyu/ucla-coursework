`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:49:10 10/28/2021 
// Design Name: 
// Module Name:    debouncer 
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
module debouncer(clk, pause_btn, rst_btn, adj_sw, sel_sw, PAUSE, RESET, ADJ, SEL
    );
	
	input clk, pause_btn, rst_btn, adj_sw, sel_sw;
	
	output PAUSE, RESET, ADJ, SEL;

	debounce pause(
		.clk(clk), .signal(pause_btn), .output_signal(PAUSE)
	);
	
	debounce rst(
		.clk(clk), .signal(rst_btn), .output_signal(RESET)
	);
	
   assign ADJ = adj_sw;
   assign SEL = sel_sw;
    
	/*debounce adj(
		.clk(clk), .signal(adj_sw), .output_signal(ADJ)
	);
	
	debounce sel(
		.clk(clk), .signal(sel_sw), .output_signal(SEL)
	);
    */

endmodule
