// Empty top module

module top (hz100, reset, pb, ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right, red, green, blue);
  input hz100, reset;
  input [20:0] pb;
  output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right;
  output red, green, blue;
  
  wire [3:0] num;
  wire [6:0] ssout;
  wire strb;

  // Your code goes here...
  ssdemux ssd (.in(ssout), .sel(num[2:0]), .out0(ss0[7:0]), .out1(ss1[7:0]), .out2(ss2[7:0]), .out3(ss3[7:0]), .out4(ss4[7:0]), .out5(ss5[7:0]), .out6(ss6[7:0]), .out7(ss7[7:0]));
  prienc pe (.out(num), .in(pb[15:0]), .strobe(strb));
  ssdec sdc (.in(num[3:0]), .enable(strb), .out(ssout[6:0]));
  
 
endmodule

module ssdemux (out7, out6, out5, out4, out3, out2, out1, out0, sel, in);
  input [2:0] sel;
  input [7:0] in;
  output [7:0] out7, out6, out5, out4, out3, out2, out1, out0;
  
  assign out0 = (sel == 3'b000)? in : 0; 
  assign out1 = (sel == 3'b001)? in : 0; 
  assign out2 = (sel == 3'b010)? in : 0; 
  assign out3 = (sel == 3'b011)? in : 0; 
  assign out4 = (sel == 3'b100)? in : 0; 
  assign out5 = (sel == 3'b101)? in : 0; 
  assign out6 = (sel == 3'b110)? in : 0; 
  assign out7 = (sel == 3'b111)? in : 0; 
  
endmodule

module prienc (out, strobe, in);
  input [15:0] in;
  output [3:0] out;
  output strobe;
  
  assign {out,strobe} = (in[15] == 1)? 5'b11111:
                        (in[14] == 1)? 5'b11101: 
                        (in[13] == 1)? 5'b11011:
                        (in[12] == 1)? 5'b11001:
                        (in[11] == 1)? 5'b10111:
                        (in[10] == 1)? 5'b10101:
                        (in[9] == 1)? 5'b10011:
                        (in[8] == 1)? 5'b10001:
                        (in[7] == 1)? 5'b01111:
                        (in[6] == 1)? 5'b01101:
                        (in[5] == 1)? 5'b01011:
                        (in[4] == 1)? 5'b01001:
                        (in[3] == 1)? 5'b00111:
                        (in[2] == 1)? 5'b00101:
                        (in[1] == 1)? 5'b00011:
                         (in[0] == 1)? 5'b00001: 0;
endmodule


module ssdec (in, enable, out);
  input [3:0] in;
  input enable;
  output [6:0] out;
  
  wire [15:0] y;

  
  assign out[0] = enable & ~(y[1] | y[4] | y[11] | y[13]);
  assign out[1] = enable & ~(y[5] | y[6] | y[11] | y[12]| y[14] | y[15]);
  assign out[2] = enable & ~(y[2] | y[12] | y[14] | y[15]);
  assign out[3] = enable & ~(y[1] | y[4] | y[7] | y[10] | y[15]);
  assign out[4] = enable & ~(y[1] | y[3] | y[4] | y[5] | y[7] | y[9]);
  assign out[5] = enable & ~(y[1] | y[2] | y[3] | y[7] | y[13]);
  assign out[6] = enable & ~(y[0] | y[1] | y[7]| y[12]);
  
  assign y =   {  enable &  in[3] &  in[2] &  in[1] &  in[0],
                  enable &  in[3] &  in[2] &  in[1] & ~in[0],
                  enable &  in[3] &  in[2] & ~in[1] &  in[0],
                  enable &  in[3] &  in[2] & ~in[1] & ~in[0],
                  enable &  in[3] & ~in[2] &  in[1] &  in[0],
                  enable &  in[3] & ~in[2] &  in[1] & ~in[0],
                  enable &  in[3] & ~in[2] & ~in[1] &  in[0],
                  enable &  in[3] & ~in[2] & ~in[1] & ~in[0],
                  enable & ~in[3] &  in[2] &  in[1] &  in[0],
                  enable & ~in[3] &  in[2] &  in[1] & ~in[0],
                  enable & ~in[3] &  in[2] & ~in[1] &  in[0],
                  enable & ~in[3] &  in[2] & ~in[1] & ~in[0],
                  enable & ~in[3] & ~in[2] &  in[1] &  in[0],
                  enable & ~in[3] & ~in[2] &  in[1] & ~in[0],
                  enable & ~in[3] & ~in[2] & ~in[1] &  in[0],
                  enable & ~in[3] & ~in[2] & ~in[1] & ~in[0] };
endmodule
