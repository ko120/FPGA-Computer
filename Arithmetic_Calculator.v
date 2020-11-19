// Empty top module

module top (hz100, reset, pb, ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right, red, green, blue);
  input hz100, reset;
  input [20:0] pb;
  output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right;
  output red, green, blue;

  // Your code goes here...
  reg [31:0] entry;
  reg [31:0] saved;
  reg [4:0] op;
  reg displaysaved;
  reg equalpressed;
  wire [4:0] key;
  wire pressed;
  reg [31:0] nextresult;
  wire [31:0] pos = displaysaved ? saved : entry;
  wire isneg = pos[31:28] ==9;
  wire [31:0] neg;
  bcd_sub32 inverter(.x(0), .y(pos), .s(neg));
  wire [31:0] num = isneg ? neg : pos;
  
  scankey sk(.clk(hz100), .reset(reset), .in({pb[19:16],6'b0,pb[9:0]}), .strobe(pressed), .out(key));
  wire [6:0] s7;
  
  assign ss7 = isneg== 1 ? 8'b01000000 : s7;
  ssdec s07 (.in(num[31:28]), .enable(|num[31:28]), .out(s7));
  ssdec s06 (.in(num[27:24]), .enable(|num[31:24]), .out(ss6));
  ssdec s05 (.in(num[23:20]), .enable(|num[31:20]), .out(ss5));
  ssdec s04 (.in(num[19:16]), .enable(|num[31:16]), .out(ss4));
  ssdec s03 (.in(num[15:12]), .enable(|num[31:12]), .out(ss3));
  ssdec s02 (.in(num[11:8]),  .enable(|num[31:8]), .out(ss2));
  ssdec s01 (.in(num[7:4]),   .enable(|num[31:4]), .out(ss1));
  ssdec s00 (.in(num[3:0]),   .enable(1'b1), .out(ss0));
  
  wire [31:0] addresult;
  wire [31:0] subresult;
  bcd_add32 add (.x(saved),.y(entry),.s(addresult));
  bcd_sub32 sub (.x(saved),.y(entry),.s(subresult));
  
  always_comb begin
  case(op)
  5'b0: begin
  nextresult = entry;
  end
  5'b10011: begin
  nextresult = addresult;
  end
  5'b10010: begin
  nextresult = subresult;
  end
  
  default : nextresult = saved;
  endcase
  end
  
  
  always @(posedge reset, posedge pressed)begin
    if(reset == 1'b1) begin
      entry <= 32'b0;
      saved <= 32'b0;
      op <= 5'b0;
      displaysaved <= 1'b0;
      equalpressed <= 1'b0;
      end
    else begin
    casez(key) 
    5'b10001:begin
      if (displaysaved == 1'b0)begin
      entry = entry >> 4;
      end
      end
    5'b10000:begin
      saved <= nextresult;
      displaysaved <= 1'b1;
      equalpressed <= 1'b1;
      end
    5'b0????:begin
      if(equalpressed == 1'b1) begin
         entry <= {28'b0, key[3:0]};
         saved <= 32'b0;
         displaysaved <= 1'b0;
         equalpressed <= 1'b0;
         op <= 5'b0;
        end
      else begin
         if(entry[31:28] == 4'b0) begin
          entry <= entry << 4 | key;
          end
          displaysaved <= 1'b0;
          end
        end 
    5'b1001?: begin
      if (equalpressed == 1'b0) begin
        saved <= nextresult;
       end
       op <= key;
       displaysaved <= 1'b1;
       entry <= 32'b0;
       equalpressed <= 1'b0;
       end
       endcase
      end 
    
    end
    
    
    
endmodule

// Add more modules down here...

module bcd_add32(x,y,s);
input [31:0] x,y;
output [31:0] s;


wire c8, c16, c32;
  bcda8 a1 (.x(x[7:0]),   .y(y[7:0]),   .cin(1'b0), .cout(c8), .s(s[7:0]));
  bcda8 a2 (.x(x[15:8]),  .y(y[15:8]),  .cin(c8), .cout(c16), .s(s[15:8]));
  bcda8 a3 (.x(x[23:16]), .y(y[23:16]), .cin(c16), .cout(c32), .s(s[23:16]));
  bcda8 a4 (.x(x[31:24]), .y(y[31:24]), .cin(c32), .cout(cout), .s(s[31:24]));
endmodule

module bcd_sub32(x,y,s);
input [31:0] x,y;
output [31:0] s;


wire c8, c16, c32;
  bcda8 a1 (.x(x[7:0]),   .y(8'h99-y[7:0]),   .cin(1'b1), .cout(c8), .s(s[7:0]));
  bcda8 a2 (.x(x[15:8]),  .y(8'h99-y[15:8]),  .cin(c8), .cout(c16), .s(s[15:8]));
  bcda8 a3 (.x(x[23:16]), .y(8'h99-y[23:16]), .cin(c16), .cout(c32), .s(s[23:16]));
  bcda8 a4 (.x(x[31:24]), .y(8'h99-y[31:24]), .cin(c32), .cout(cout), .s(s[31:24]));
endmodule


module scankey (clk, reset, in, strobe, out);
    
  input clk, reset, strobe;
  input [19:0] in;
  output [4:0] out;
  
  reg [1:0] delay; 
  wire delayIn;
  
  always @(posedge clk, posedge reset)begin
     if (reset == 1'b1)begin
     delay <= 2'b0;
     end
     else begin
     delay <= (delay<<1) | delayIn;
     end
     end
     
  assign strobe = delay[1];
  assign delayIn = in[0] | out[0] | out[1] | out[2] | out [3] | out [4];
  assign out[0] = in[1] | in[3] | in[5] | in[7] | in[9] | in[11] | in[13] | in[15]| in[17] | in[19]; 
  assign out[1] = in[2] | in[3] | in[6] | in[10] | in[11] | in[14] | in[15] | in[18]| in[19];
  assign out[2] = in[4] | in[5] | in[6] | in[7] | in[12] | in[13] | in[14] | in[15]| in[20];
  assign out[3] = in[8] | in[9] | in[10] | in[11] | in[12] | in[13] | in[14] | in[15];
  assign out[4] = in[16] | in[17] | in[18] | in[19] | in[20];
  
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

module fa(x, y, cin, cout, s);
input x,y, cin;
output s, cout;

  assign s = x ^ y ^cin;
  assign cout = x & y | x & cin | y & cin;
  
endmodule 


module fa4 (x, y, cin, cout, s);

input [3:0] x,y;
input cin;
output [3:0] s;
output cout;

wire a,b,c;
  fa a1(.x(x[0]), .y(y[0]), .cin(cin), .cout(a),    .s(s[0]));
  fa a2(.x(x[1]), .y(y[1]), .cin(a),   .cout(b),    .s(s[1]));
  fa a3(.x(x[2]), .y(y[2]), .cin(b),   .cout(c),    .s(s[2]));
  fa a4(.x(x[3]), .y(y[3]), .cin(c),   .cout(cout), .s(s[3]));
 
endmodule

module fa8 (x, y, cin, cout, s);
input [7:0] x,y;
input cin;
output cout;
output [7:0] s;

wire a;

  fa4 a1(.x(x[3:0]), .y(y[3:0]), .cin(cin), .cout(a),    .s(s[3:0]));
  fa4 a2(.x(x[7:4]), .y(y[7:4]), .cin(a), .cout(cout),   .s(s[7:4]));

endmodule

module bcda4 (x, y, cin , cout, s);
input [3:0] x,y;
input cin;
output cout;
output [3:0] s;

wire [4:0]z;

  fa4 a1(.x(x[3:0]), .y(y[3:0]), .cin(cin), .cout(z[4]),  .s(z[3:0]));
  assign cout = z[4] | z[3] & z[2] | z[3] & z[1];
  fa4 a2(.x({1'b0, cout, cout, 1'b0}), .y(z[3:0]), .cin(0), .s(s)); 

endmodule 

module bcda8 (x, y, cin, cout,s);
input [7:0] x,y;
output [7:0]s;
input cin;
output cout;

wire a;

  bcda4 a1 (.x(x[3:0]), .y(y[3:0]), .cin(cin), .cout(a), .s(s[3:0]));
  bcda4 a2 (.x(x[7:4]), .y(y[7:4]), .cin(a), .cout(cout), .s(s[7:4]));  
  
endmodule
