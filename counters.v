// Empty top module

module top (hz100, reset, pb, ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right, red, green, blue);
  input hz100, reset;
  input [20:0] pb;
  output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right;
  output red, green, blue;


  wire [7:0] TQ;
  wire [7:0] SQ; //step 2 Q
  wire [2:0] RQ; //step 3 Q
  // Your code goes here...
  assign left = TQ;
  countn div (.CLK(hz100), .MAX(8'd24), .Q(SQ), .EN(1'b1), .AR(reset));
  count8du c (.E(pb[2]), .M(pb[1]), .AR(reset), .CLK(SQ[4]), .Q(TQ));
 /* decode_led d0 (.ss(ss0), .value(TQ[3:0]) , .enable(1'b1));
  decode_led d1 (.ss(ss1), .value(TQ[7:4]) , .enable(1'b1));
  */
  updown3 s (.CLK(SQ[4]), .AR(reset), .Q(RQ));
  display_ball d (.pos(RQ), .ss0(ss0), .ss1(ss1), .ss2(ss2), .ss3(ss3), .ss4(ss4), .ss5(ss5), .ss6(ss6),.ss7(ss7));
  
endmodule

module updown3 (AR, CLK, Q);
  input AR, CLK;
  output reg [2:0] Q;
  
  reg [2:0] Q2;
  reg dir;
  
  always @ (posedge CLK, posedge AR) begin
  if (AR == 1'b1)begin
   Q <= 3'b000;
   dir <= 1'b0;
   end
    else if (dir == 1'b1 & Q == 3'b000) begin
    dir <= 1'b0;
    Q <= 3'b001;
    end
    else if (dir == 1'b0 && Q== 3'b111) begin
    dir <= 1'b1;
    Q <= 3'b110;
    end 
    else begin
    Q <= Q2;
     end
  end

  always @(Q) begin
    if(dir == 1'b1) begin
    Q2[0] = ~Q[0];
    Q2[1] = Q[1] ^ ~Q[0];
    Q2[2] = Q[2] ^ (~Q[1] & ~Q[0]);
    end
    else begin
    Q2[0] = ~Q[0];
    Q2[1] = Q[1] ^ Q[0];
    Q2[2] = Q[2] ^ (Q[1] & Q[0]);
    end
    end

    endmodule
    
module display_ball (pos, ss0, ss1, ss2, ss3, ss4, ss5, ss6,ss7);

input [2:0] pos;
output [7:0] ss0;
output [7:0] ss1;
output [7:0] ss2;
output [7:0] ss3;
output [7:0] ss4;
output [7:0] ss5;
output [7:0] ss6;
output [7:0] ss7;

assign ss0 = (pos == 3'b000) ? 8'b01011100: 0;
assign ss1 = (pos == 3'b001) ? 8'b01011100: 0;
assign ss2 = (pos == 3'b010) ? 8'b01011100: 0;
assign ss3 = (pos == 3'b011) ? 8'b01011100: 0;
assign ss4 = (pos == 3'b100) ? 8'b01011100: 0;
assign ss5 = (pos == 3'b101) ? 8'b01011100: 0;
assign ss6 = (pos == 3'b110) ? 8'b01011100: 0;
assign ss7 = (pos == 3'b111) ? 8'b01011100: 0;

endmodule
  

module countn(CLK, MAX, Q, EN, AR);
input CLK, EN, AR;
input [7:0]MAX;
output reg[7:0]Q;

reg [7:0] Q2;

always @(posedge CLK, posedge AR) begin
  if (AR == 1'b1) begin
  Q <= 8'b00000000;
  end
  else if(EN == 1'b1) begin
  Q <= Q2;
  end
  end
  
  always @ (Q) begin
    if (Q == MAX) begin
    Q2 = 8'b0;
    end
    else begin
    Q2[0] = ~Q[0];
    Q2[1] = Q[1] ^ Q[0];
    Q2[2] = Q[2] ^ (Q[1] & Q[0]);
    Q2[3] = Q[3] ^ (Q[2] & Q[1] & Q[0]);
    Q2[4] = Q[4] ^ (Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[5] = Q[5] ^ (Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[6] = Q[6] ^ (Q[5] & Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[7] = Q[7] ^ (Q[6] & Q[5] & Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    end 
    end
endmodule

module count8du(E, M, AR, CLK, Q);
  input E, AR, CLK, M;
  output reg  [7:0] Q;
  reg [7:0] Q2;
  
  always @(posedge CLK, posedge AR) begin
    if (AR == 1'b1) begin
    Q<= 8'b00000000;
    end
    else if (E == 1'b1) begin
    Q <= Q2;
    end
  end
  always @(Q,M) begin
    if (M== 1'b0) begin
    Q2[0] = ~Q[0];
    Q2[1] = Q[1] ^ ~Q[0];
    Q2[2] = Q[2] ^ (~Q[1] & ~Q[0]);
    Q2[3] = Q[3] ^ (~Q[2] & ~Q[1] & ~Q[0]);
    Q2[4] = Q[4] ^ (~Q[3] & ~Q[2] & ~Q[1] & ~Q[0]);
    Q2[5] = Q[5] ^ (~Q[4] & ~Q[3] & ~Q[2] & ~Q[1] & ~Q[0]);
    Q2[6] = Q[6] ^ (~Q[5] & ~Q[4] & ~Q[3] & ~Q[2] & ~Q[1] & ~Q[0]);
    Q2[7] = Q[7] ^ (~Q[6] & ~Q[5] & ~Q[4] & ~Q[3] & ~Q[2] & ~Q[1] & ~Q[0]);
    end
    else begin
    Q2[0] = ~Q[0];
    Q2[1] = Q[1] ^ Q[0];
    Q2[2] = Q[2] ^ (Q[1] & Q[0]);
    Q2[3] = Q[3] ^ (Q[2] & Q[1] & Q[0]);
    Q2[4] = Q[4] ^ (Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[5] = Q[5] ^ (Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[6] = Q[6] ^ (Q[5] & Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    Q2[7] = Q[7] ^ (Q[6] & Q[5] & Q[4] & Q[3] & Q[2] & Q[1] & Q[0]);
    end
    end

    endmodule
    
module decode_led(ss, value, enable);
  input [3:0] value;
  output [7:0] ss;
  input enable;

  wire [15:0] y;

  
  assign ss[0] = enable & ~(y[1] | y[4] | y[11] | y[13]);
  assign ss[1] = enable & ~(y[5] | y[6] | y[11] | y[12]| y[14] | y[15]);
  assign ss[2] = enable & ~(y[2] | y[12] | y[14] | y[15]);
  assign ss[3] = enable & ~(y[1] | y[4] | y[7] | y[10] | y[15]);
  assign ss[4] = enable & ~(y[1] | y[3] | y[4] | y[5] | y[7] | y[9]);
  assign ss[5] = enable & ~(y[1] | y[2] | y[3] | y[7] | y[13]);
  assign ss[6] = enable & ~(y[0] | y[1] | y[7]| y[12]);
  

  assign y =   {  enable &  value[3] &  value[2] &  value[1] &  value[0],
                  enable &  value[3] &  value[2] &  value[1] & ~value[0],
                  enable &  value[3] &  value[2] & ~value[1] &  value[0],
                  enable &  value[3] &  value[2] & ~value[1] & ~value[0],
                  enable &  value[3] & ~value[2] &  value[1] &  value[0],
                  enable &  value[3] & ~value[2] &  value[1] & ~value[0],
                  enable &  value[3] & ~value[2] & ~value[1] &  value[0],
                  enable &  value[3] & ~value[2] & ~value[1] & ~value[0],
                  enable & ~value[3] &  value[2] &  value[1] &  value[0],
                  enable & ~value[3] &  value[2] &  value[1] & ~value[0],
                  enable & ~value[3] &  value[2] & ~value[1] &  value[0],
                  enable & ~value[3] &  value[2] & ~value[1] & ~value[0],
                  enable & ~value[3] & ~value[2] &  value[1] &  value[0],
                  enable & ~value[3] & ~value[2] &  value[1] & ~value[0],
                  enable & ~value[3] & ~value[2] & ~value[1] &  value[0],
                  enable & ~value[3] & ~value[2] & ~value[1] & ~value[0] };
endmodule
