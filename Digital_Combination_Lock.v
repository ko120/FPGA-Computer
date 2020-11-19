// Empty top module

module top (hz100, reset, pb, ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right, red, green, blue);
  input hz100, reset;
  input [20:0] pb;
  output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right;
  output red, green, blue;
  
  wire [7:0] disp_en;
  wire [4:0] keycode;
  wire strobe;
  wire [1:0] state;
  wire hz2;
  
  clock_hz2 cldiv (.clk(hz100), .reset(reset), .hz2(hz2));
  scankey sk1 (.clk(hz100), .reset(reset), .in(pb[19:0]), .strobe(strobe), .out(keycode));
  display_enable_handler deh1 (.clk(strobe), .reset(reset), .keycode(keycode), .enable_out(disp_en));
  dcl_controller dcl1 (.clk(strobe), .reset(reset), .enable(keycode == 5'b10000), .state(state));
   
  localparam SETPASS = 0;
  localparam ENTRY = 1;
  localparam VERIFY = 2;
  
  
  wire [31:0] ssdec_in;
  wire [31:0] passwd_save;
  wire [31:0] passwd_entry;
  wire password_entry_enable;
  
  passwd p1 (.clk(strobe), .reset(reset), .keycode(keycode),.enable(state == SETPASS), .out(passwd_save));
  passwd p2 (.clk(strobe), .reset(reset), .keycode(keycode),.enable(state == ENTRY), .out(passwd_entry));
  //since left and right can't show more than 8 bits
  //assign left = passwd_save [7:0];
  //assign right = passwd_entry [7:0];
  // Turn on the blue LED when 'state' is one
  assign blue = state == ENTRY;
  // Tell us if the values matched
  assign open = state == VERIFY ? passwd_save == passwd_entry: 0;
  assign alarm = state == VERIFY ? passwd_save != passwd_entry: 0;
  wire [63:0] ss_status;
  wire [63:0] ss_decout;
  // Flash red or assert green correspondingly
  assign red = alarm & hz2;
  assign green = open;
  assign ssdec_in = state == SETPASS ? passwd_save: passwd_entry;
  assign password_entry_enable = (state != VERIFY) & (|disp_en) ? 1: 0;
  assign {ss7,ss6,ss5,ss4,ss3,ss2,ss1,ss0} = password_entry_enable == 1'b1 ? ss_decout : ss_status;
 
  ssdec a (.in(ssdec_in [3:0]),   .enable(disp_en[0]), .out(ss_decout[7:0]));
  ssdec b (.in(ssdec_in [7:4]),   .enable(disp_en[1]), .out(ss_decout[15:8]));
  ssdec c (.in(ssdec_in [11:8]),  .enable(disp_en[2]), .out(ss_decout[23:16]));
  ssdec d (.in(ssdec_in [15:12]), .enable(disp_en[3]), .out(ss_decout[31:24]));
  ssdec e (.in(ssdec_in [19:16]), .enable(disp_en[4]), .out(ss_decout[39:32]));
  ssdec f (.in(ssdec_in [23:20]), .enable(disp_en[5]), .out(ss_decout[47:40]));
  ssdec g (.in(ssdec_in [27:24]), .enable(disp_en[6]), .out(ss_decout[55:48]));
  ssdec h (.in(ssdec_in [31:28]), .enable(disp_en[7]), .out(ss_decout[63:56]));
  status_display display (state, open, alarm, ss_status);
endmodule

module passwd(clk, reset, keycode, enable, out);
  input clk, reset, enable;
  input [4:0] keycode;
  output reg [31:0] out;
  
  reg [31:0] out2;
  
  always @ (posedge clk, posedge reset) begin
    if(reset == 1'b1) begin
      out <= 32'b0;
    end
    else if (enable == 1'b1) begin
      out <= out2;
    end
    end
    
  always@(out) begin
    if(keycode == 5'b10001)begin
    
     out2 = out >> 4;
    end
    else if (keycode[4] == 0) begin
      out2 = out << 4 | keycode[3:0];
    end
    else begin
    out2 = out;
    end
  end  
  endmodule
  
  
module status_display(state, open, error, ss);
  input [1:0] state;
  input open, error;
  output reg [63:0]ss;
  
  wire [7:0] charS = 8'b01101101;
  wire [7:0] charE = 8'b01111001;
  wire [7:0] charC = 8'b00111001;
  wire [7:0] charU = 8'b00111110;
  wire [7:0] charR = 8'b01010000;
  wire [7:0] charO = 8'b00111111;
  wire [7:0] charP = 8'b01110011;
  wire [7:0] charN = 8'b01010100;
  wire [7:0] char9 = 8'b01101111;
  wire [7:0] char1 = 8'b00000110;
  wire [7:0] charA = 8'b01110111;
  wire [7:0] charL = 8'b00111000;
  wire [7:0] charI = 8'b00110000;
  wire [7:0] blank = 8'b00000000;


  localparam SETPASS = 0;
  localparam ENTRY = 1;
  localparam VERIFY = 2;
  
  always @ (state) begin
    if (state == SETPASS) begin
      ss = {8{blank}};
    end
    else if (state == ENTRY) begin
      ss = {charS,charE,charC,charU,charR,charE,blank,blank};
    end
    else if((state == VERIFY) && open) begin
      ss = {charO,charP,charE,charN,blank,blank,blank,blank};
    end
    else if((state == VERIFY) && error) begin
      ss = {charC,charA,charL,charL,blank,char9,char1,char1};
    end
    else begin
      ss = {8{blank}};
    end
    end
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
  
module clock_hz2 (clk, reset, hz2);
  input clk, reset;
  output [7:0] hz2;

  wire [7:0] mod;
  reg [7:0] Q2;
  reg [7:0] Q;
  assign mod = 8'd50;
  assign hz2 = Q[5];

  always@(posedge clk, posedge reset)begin
    if (reset == 1'b1) begin
    Q <= 8'b0;
    end
    else begin
    Q <= Q2;
    end
    end
  always@(Q) begin
    if (Q == mod)begin
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

module display_enable_handler(clk, reset, keycode, enable_out);
  input clk, reset;
  input [4:0] keycode;
  output reg [7:0] enable_out;

  reg [7:0] enable_out2; 

  always @(posedge clk, posedge reset)begin
    if (reset == 1'b1)begin
    enable_out <=8'b0;
    end
    else begin
    enable_out <= enable_out2;
    end
  end
  
  always@(enable_out)begin
    if (keycode == 5'b10000)begin
    enable_out2 = 8'b0;
    end
    else if (keycode == 5'b10001)begin
    //enable_out2 = {1'b0, enable_out[7:1]};
    enable_out2 = enable_out >> 1;
    end
    else if (keycode[4] ==0) begin
    enable_out2 = (enable_out<<1) | 1'b1;
    end
    else begin
    enable_out2 = enable_out;
    end
    
  end
endmodule

module dcl_controller (clk, reset, enable, state);
  input clk, reset, enable;
  output reg [1:0] state;

  reg [1:0] state2;

  localparam SETPASS = 2'b00;
  localparam ENTRY = 2'b01;
  localparam VERIFY = 2'b10;

  always @(posedge clk, posedge reset)begin
    if(reset == 1'b1) begin
      state <= SETPASS;
      end
    else if (enable == 1'b1)begin
      state <= state2;
    end
    end
    
  always @(state)begin
    case(state)
      SETPASS:
        state2 = ENTRY;
      ENTRY:
        state2 = VERIFY;
      VERIFY:
        state2 = state;
      default: state2 = state;
      endcase
    end
    endmodule
  
// Add more modules down here...
