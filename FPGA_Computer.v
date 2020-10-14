// Empty top module

module top (hz100, reset, pb, ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right,
 red, green, blue, txdata, txclk, txready, rxdata, rxclk, rxready);
 
 input hz100, reset, rxready, txready;
 input [20:0] pb;
 output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right, txdata;
 output red, green, blue, rxclk, txclk;
 input [7:0] rxdata;
 wire clk = hz100;
 wire [31:0] irout, r1, r2, r3;
 wire [3:0] flags;
 wire [3:0] rsel1, rsel2, rsel3, wsel;
 wire rwe, fue, mwe, irl, iml, pcc;
 wire [4:0] aop;
 wire [1:0] msize;
 wire regmuxsel;
 wire [1:0] alumuxsel, addrmuxsel;
 wire [31:0] regmux, alumux, memout, aluout;
 wire [15:0] addrmux, pcout;
 
 localparam REGMUX_ALU = 0; localparam REGMUX_MEM = 1;
 assign regmux = regmuxsel == REGMUX_ALU ? aluout :
 regmuxsel == REGMUX_MEM ? memout : 32'b0;
 
 regs r(.reset(reset), .clk(clk), .wsel(wsel), .wen(rwe), .w(regmux),
 .rsel1(rsel1), .r1(r1), .rsel2(rsel2), .r2(r2), .rsel3(rsel3), .r3(r3));
 
 localparam ADDRMUX_ALU = 0; localparam ADDRMUX_IMM16 = 1; localparam ADDRMUX_PC = 2;
 assign addrmux = addrmuxsel == ADDRMUX_ALU ? aluout :
 addrmuxsel == ADDRMUX_IMM16 ? irout[15:0] :
 addrmuxsel == ADDRMUX_PC ? pcout : 16'b0;
 
 wire mem_write = mwe && addrmux[15:14] == 0;
 mem m(.clk(clk), .size(msize), .waddr(addrmux), .wen(mwe), .w(aluout),
 .raddr(addrmux), .r(memout));
 assign txclk = mwe && addrmux == 16'hff00;
 assign txdata = aluout[7:0];
 pc p(.clk(clk), .reset(reset), .pcc(pcc), .addr(pcout));
 
 ir ir(.clk(clk), .reset(reset), .irl(irl), .iml(iml),
 .data(memout[15:0]), .out(irout));
 
 localparam ALUMUX_R2 = 0; localparam ALUMUX_IMM8 = 1; localparam ALUMUX_IMM16 = 2;
 assign alumux = alumuxsel == ALUMUX_R2 ? r2 :
 alumuxsel == ALUMUX_IMM8 ? {24'b0,irout[23:16]} :
 alumuxsel == IMM16 ? {16'b0,irout[15:0]} : 32'b0;
 
 alu a(.aop(aop), .in1(r1), .in2(alumux), .out(aluout),
 .clk(clk), .flags(flags), .fue(fue));
 
 idms idms(.clk(clk), .reset(reset), .inst(irout[31:16]), .flags(flags),
 .rsel1(rsel1), .rsel2(rsel2), .wsel(wsel), .rwe(rwe), .aop(aop),.fue(fue),
 .mwe(mwe), .msize(msize), .irl(irl), .iml(iml), .pcc(pcc),
 .regmuxsel(regmuxsel), .alumuxsel(alumuxsel), .addrmuxsel(addrmuxsel));
 
 reg [31:0] out;
 reg [1:0] size;
 reg [7:0] ctrl;
 ssdec s0(.in(out[3:0]), .out(ss0), .en(1));
 ssdec s1(.in(out[7:4]), .out(ss1), .en(1));
 ssdec s2(.in(out[11:8]), .out(ss2), .en(|size));
 ssdec s3(.in(out[15:12]), .out(ss3), .en(|size));
 ssdec s4(.in(out[19:16]), .out(ss4), .en(size[1]));
 ssdec s5(.in(out[23:20]), .out(ss5), .en(size[1]));
 ssdec s6(.in(out[27:24]), .out(ss6), .en(size[1]));
 ssdec s7(.in(out[31:28]), .out(ss7), .en(size[1]));
 localparam DEBUGADDR = 0; localparam DEBUGMEM = 1;
 localparam DEBUGIR = 2; localparam DEBUGR1 = 3;
 localparam DEBUGR2 = 4; localparam DEBUGALU = 5;
 localparam DEBUGW = 6; localparam DEBUGREGS = 7;
 reg [2:0]mode;
 always_ff @(posedge pb[16], posedge reset)
 if (reset)
 mode <= 0;
 else
 mode <= mode+1;
 assign right = { mode==7,mode==6,mode==5,mode==4,
mode==3,mode==2,mode==1,mode==0};
 assign left = ctrl;
 reg [1:0] prev_msize;
 always_ff @(posedge clk)
 prev_msize <= msize;

always_comb
 case(mode)
 DEBUGADDR: begin out = addrmux;
 size = 1;
ctrl = {addrmuxsel,2'b0,pcc,mwe,msize};
 end
 DEBUGMEM: begin out = memout;
 size = prev_msize;
ctrl = {6'b0,msize};
 end
 DEBUGIR: begin out = irout;
 size = 2;
ctrl = {6'b0,iml,irl};
 end
 DEBUGR1: begin out = r1;
 size = 2;
ctrl = {4'b0,rsel1};
 end
 DEBUGR2: begin out = r2;
 size = 2;
ctrl = {4'b0,rsel2};
 end
 DEBUGALU: begin out = aluout;
 size = 2;
ctrl = {alumuxsel,fue,aop}; 
 end
 DEBUGW: begin out = regmux;
 size = 2;
ctrl = {regmuxsel,2'b0,rwe,wsel} ; 
 end
 DEBUGREGS: begin out = r3;
 size = 2;
ctrl = rsel3;
 end
 endcase
 encode16to4 e(.in(pb[15:0]), .out(rsel3));
 
 

endmodule




module encode16to4 (in ,out);
  input [15:0] in;
  output [3:0] out;
  
  
  assign {out} = (in[15] == 1)? 5'b11111:
                        (in[14] == 1)? 4'b1110: 
                        (in[13] == 1)? 4'b1101:
                        (in[12] == 1)? 4'b1100:
                        (in[11] == 1)? 4'b1011:
                        (in[10] == 1)? 4'b1010:
                        (in[9] == 1)? 4'b1001:
                        (in[8] == 1)? 4'b1000:
                        (in[7] == 1)? 4'b0111:
                        (in[6] == 1)? 4'b0110:
                        (in[5] == 1)? 4'b0101:
                        (in[4] == 1)? 4'b0100:
                        (in[3] == 1)? 4'b0011:
                        (in[2] == 1)? 4'b0010:
                        (in[1] == 1)? 4'b0001:
                         (in[0] == 1)? 4'b0000: 0;
endmodule


// Add more modules down here...
module ssdec(out, in, en);
    output wire [7:0] out; 
    input  wire [3:0] in;  
    input  wire  en;

  wire [15:0] y;

  
  assign out[0] = en & ~(y[1] | y[4] | y[11] | y[13]);
  assign out[1] = en & ~(y[5] | y[6] | y[11] | y[12]| y[14] | y[15]);
  assign out[2] = en & ~(y[2] | y[12] | y[14] | y[15]);
  assign out[3] = en & ~(y[1] | y[4] | y[7] | y[10] | y[15]);
  assign out[4] = en & ~(y[1] | y[3] | y[4] | y[5] | y[7] | y[9]);
  assign out[5] = en & ~(y[1] | y[2] | y[3] | y[7] | y[13]);
  assign out[6] = en & ~(y[0] | y[1] | y[7]| y[12]);
  assign y =   {  en &  in[3] &  in[2] &  in[1] &  in[0],
                  en &  in[3] &  in[2] &  in[1] & ~in[0],
                  en &  in[3] &  in[2] & ~in[1] &  in[0],
                  en &  in[3] &  in[2] & ~in[1] & ~in[0],
                  en &  in[3] & ~in[2] &  in[1] &  in[0],
                  en &  in[3] & ~in[2] &  in[1] & ~in[0],
                  en &  in[3] & ~in[2] & ~in[1] &  in[0],
                  en &  in[3] & ~in[2] & ~in[1] & ~in[0],
                  en & ~in[3] &  in[2] &  in[1] &  in[0],
                  en & ~in[3] &  in[2] &  in[1] & ~in[0],
                  en & ~in[3] &  in[2] & ~in[1] &  in[0],
                  en & ~in[3] &  in[2] & ~in[1] & ~in[0],
                  en & ~in[3] & ~in[2] &  in[1] &  in[0],
                  en & ~in[3] & ~in[2] &  in[1] & ~in[0],
                  en & ~in[3] & ~in[2] & ~in[1] &  in[0],
                  en & ~in[3] & ~in[2] & ~in[1] & ~in[0] };

endmodule

module regs (reset, clk, wsel, wen, w, rsel1, r1, rsel2, r2, rsel3, r3);
  input reset, clk;
  input [3:0] wsel, rsel1, rsel2, rsel3;
  input wen;
  input [31:0] w;
  output reg [31:0] r1, r2, r3;
  
  reg [31:0] r[15:0];
  
  assign r1 = r[rsel1];
  assign r2 = r[rsel2];
  assign r3 = r[rsel3];
  
  always_ff @(posedge clk, posedge reset)
    if (reset == 1'b1) begin
      r[0] <= 0; r[1]  <= 0; r[2] <= 0; r[3] <=0;
      r[4] <= 0; r[5]  <= 0; r[6] <= 0; r[7] <=0;
      r[8] <= 0; r[9]  <= 0; r[10] <= 0; r[11] <=0;
      r[12] <= 0; r[13]  <= 0; r[14] <= 0; r[15] <=0;
    end
    else if (wen == 1'b1)
      r[wsel] <= w;
      
  endmodule
  
 module mem (clk, size, waddr, wen, w, raddr, r);
   input clk, wen;
   input [1:0] size;
   input [13:0] waddr, raddr;
   input [31:0] w;
   output reg [31:0] r;
   reg [31:0] storage [511:0];
   reg [31:0] rdata;
   reg [13:0] sraddr;
   reg [1:0] ssize;
   
   always_ff @(posedge clk) begin
     rdata <= storage[raddr >>2];
     sraddr <= raddr[1:0];
     ssize <= size;
     
     if (wen ==1'b1) storage[waddr >>2] <= w;
     end
    assign r = ssize == 0? rdata >> ((sraddr & 2'b11) <<3):
               ssize == 1? rdata >> ((sraddr & 2'b10) <<3):
               rdata;

 
 function [31:0] two16 (input [15:0] a,b); two16 = {b,a}; endfunction
 initial begin
 
 /*
   storage [16'h0000 >>2] = two16(16'ha000, 16'h0020);
   storage [16'h0004 >>2] = two16(16'h3024, 16'hc000);
   storage [16'h0008 >>2] = two16(16'h0028, 16'ha100);
   storage [16'h000c >>2] = two16(16'h0024, 16'h8120);
   storage [16'h0010 >>2] = two16(16'hc100, 16'h002c);
   storage [16'h0014 >>2] = two16(16'hdf00, 16'h0000);
   storage [16'h0020 >>2] = 32'h81111111;
   storage [16'h0024 >>2] = 32'h22222222;
   storage [16'h0028 >>2] = 32'h00000000;
   storage [16'h002c >>2] = 32'h00000000;
   */
   
  /*
 storage[16'h0000>>2] = two16(16'h6012, 16'h3011); // LDIBU R0,#12; ADDI R0,#11
 storage[16'h0004>>2] = two16(16'h6123, 16'h3112); // LDIBU R1,#23; ADDI R1,#12
 storage[16'h0008>>2] = two16(16'h8021, 16'hdf00); // ADD R0,R1; HLT
*/
/*
 storage[16'h0000>>2] = two16(16'ha000, 16'h0020); // LDL R0,0020
 storage[16'h0004>>2] = two16(16'ha100, 16'h0024); // LDL R1,0024
 storage[16'h0008>>2] = two16(16'h8021, 16'hdf00); // ADD R0,R1; HLT
 storage[16'h0020>>2] = 32'h87654321;
 storage[16'h0024>>2] = 32'h12345678;
*/
/*
 storage[16'h0000>>2] = two16(16'h6000, 16'h4001); // LDIBU R0,#0; SUBI R0,#1
 storage[16'h0004>>2] = two16(16'hc000, 16'h0020); // STL R0,0020
 storage[16'h0008>>2] = two16(16'ha100, 16'h0020); // LDL R1,0020
 storage[16'h000c>>2] = two16(16'hdf00, 16'h0000); // HLT
 storage[16'h0020>>2] = 32'h87654321;
 storage[16'h0024>>2] = 32'h12345678;
*/

storage[16'h0000 >>2] = two16(16'h604B, 16'h6179);//K
storage[16'h0004 >>2] = two16(16'hc000, 16'hff00);
storage[16'h0008 >>2] = two16(16'hc100, 16'hff00);
storage[16'h000c >>2] = two16(16'h6075, 16'h616E);//yu
storage[16'h0010 >>2] = two16(16'hc000, 16'hff00);
storage[16'h0014 >>2] = two16(16'hc100, 16'hff00);
storage[16'h0018 >>2] = two16(16'h6067, 16'h6120);//ng
storage[16'h001c >>2] = two16(16'hc000, 16'hff00);
storage[16'h0020 >>2] = two16(16'hc100, 16'hff00);
storage[16'h0024 >>2] = two16(16'h604D, 16'h6169);//m
storage[16'h0028 >>2] = two16(16'hc000, 16'hff00);
storage[16'h002c >>2] = two16(16'hc100, 16'hff00);
storage[16'h0030 >>2] = two16(16'h606E, 16'h6120);//in
storage[16'h0034 >>2] = two16(16'hc000, 16'hff00);
storage[16'h0038 >>2] = two16(16'hc100, 16'hff00); 
storage[16'h003c >>2] = two16(16'h604B, 16'h616F);//K
storage[16'h0040 >>2] = two16(16'hc000, 16'hff00);
storage[16'h0044 >>2] = two16(16'hc100, 16'hff00);
storage[16'h0048 >>2] = two16(16'h600d, 16'h610a);//o
storage[16'h004c >>2] = two16(16'hc000, 16'hff00);
storage[16'h0050 >>2] = two16(16'hc100, 16'hff00);
storage[16'h0054 >>2] = two16(16'hdf00, 16'h0000);


 end

endmodule
  
module pc(clk, reset, pcc, addr);
  input wire clk, reset, pcc;
  output wire [15:0] addr;
  
  reg [15:0] count;
  assign addr = count;
  
  always_ff @(posedge clk, posedge reset) begin
    if (reset == 1'b1)
      count <= 15'b0;
    else if (pcc == 1'b1)
      count <= count + 2;
  end
endmodule

    
module ir(clk, reset, irl, iml, data, out);
 input wire clk, reset;
 input wire irl; 
 input wire iml; 
 input wire [15:0] data; 
 output wire [31:0] out; 
 reg [15:0] inst;
 reg [15:0] addr;

 assign out = {irl ? data : inst, iml ? data : addr};
 always_ff @ (posedge reset, posedge clk)
 if (reset == 1) begin
 inst <= 0; addr <= 0;
 end
 else if (irl == 1) inst[15:0] <= data;
 else if (iml == 1) addr[15:0] <= data;
endmodule


module alu(aop, in1, in2, out, clk, flags, fue);
  output reg [31:0] out;
  output reg [3:0] flags;
  input [31:0] in1, in2;
  input [4:0] aop;
  input clk, fue;

  reg [3:0] nflags;
  
  
  localparam ALU_ADD = 5'd0;
  localparam ALU_ADC = 5'd1;
  localparam ALU_SUB = 5'd2;
  localparam ALU_SBC = 5'd3;
  localparam ALU_NEG = 5'd4;
  localparam ALU_LSL = 5'd5;
  localparam ALU_ASR = 5'd6;
  localparam ALU_LSR = 5'd7;
  localparam ALU_ROR = 5'd8;
  localparam ALU_OR  = 5'd9;
  localparam ALU_AND = 5'd10;
  localparam ALU_BIC = 5'd11;
  localparam ALU_XOR = 5'd12;
  localparam ALU_NOT = 5'd13;
  localparam ALU_SXB = 5'd14;
  localparam ALU_SXW = 5'd15;
  localparam ALU_ZXB = 5'd16;
  localparam ALU_ZXW = 5'd17;
  localparam ALU_IN1 = 5'd18;
  localparam ALU_IN2 = 5'd19;

  wire Nin, Zin, Cin, Vin;

  
  wire Cadd, Csub, Vadd, Vsub; 
  assign flags = {Nin, Zin, Cin, Vin};
  assign Cadd = (in1[31]  && in2[31]) || (in1[31] && ~out[31] ) || (in2[31] && ~out[31]);
  assign Csub = (in1[31]  && ~in2[31]) || (in1[31] && ~out[31] ) || (~in2[31]  && ~out[31]);
  assign Vadd = (in1[31]  && in2[31] && ~out[31]) || (~in1[31] && ~in2[31] && out[31] );
  assign Vsub = (in1[31]  && ~in2[31]&&  ~out[31]) || (~in1[31]  && in2[31] && out[31]);
  assign Cneg = ~in2[31] && ~out[31];
  wire N = out[31];
  wire Z = ~(|out);
  wire C = Cin;
  wire V = Vin;


  always_comb
    case(aop)
      ALU_ADD: begin out=in1 + in2;   nflags={N,Z,Cadd,Vadd}; end

      // add the rest of the cases here...
      ALU_ADC: begin out = in1 + in2 + Cin; nflags={N,Z,Cadd,Vadd}; end
      
      ALU_SUB: begin out = in1 - in2; nflags= {N,Z,Csub,Vsub}; end 
      
      ALU_SBC: begin out = in1 - in2 + Cin; nflags = {N,Z,Csub,Vsub}; end 
      
      ALU_NEG: begin out = 0 - in2; nflags = {N,Z,Cneg,V}; end 
      
      ALU_OR: begin out = in1 | in2; nflags ={N,Z,Cin,Vin}; end
      
      ALU_AND: begin out = in1 & in2; nflags ={N,Z,Cin,Vin}; end
      
      ALU_BIC: begin out = in1 & ~in2; nflags ={N,Z,Cin,Vin}; end
      
      ALU_XOR: begin out = in1 ^ in2; nflags ={N,Z,Cin,Vin}; end
      
      ALU_NOT: begin out = ~in2; nflags ={N,Z,Cin,Vin}; end
      
      ALU_ZXB: begin out = {24'b0,in2[7:0]}; nflags ={N,Z,Cin,Vin}; end
      
      ALU_IN1: begin out = in1; nflags ={N,Z,Cin,Vin}; end
      
      ALU_IN2: begin out = in2; nflags ={N,Z,Cin,Vin}; end
      

      default: begin out=32'b0;      nflags={Nin,Zin,Cin,Vin}; end
    endcase
  
  always_ff @(posedge clk, posedge reset)
    if (reset == 1) begin
      flags <= 0;
    end
    
    else if (fue) begin
      flags <= nflags;
    end
endmodule

module idms(clk, reset, inst, flags,
 rsel1, rsel2, wsel, rwe, aop, fue, mwe, msize, irl, iml, pcc,
 regmuxsel, alumuxsel, addrmuxsel);
 input clk, reset;
 input [15:0] inst;
 input [3:0] flags;
 output rwe, mwe, fue, irl, iml, pcc;
 output [1:0] msize;
 output [3:0] rsel1, rsel2, wsel;
 output [4:0] aop;
 output [1:0] alumuxsel, addrmuxsel;
 output regmuxsel;
 wire [3:0] opcode = inst[15:12]; // opcode is the primary 4-bit instruction operation
 wire [3:0] type = inst[7:4]; // secondary opcode for 2-register instructions
 reg run; // run and sq are the only state variables in the idms.
 reg [1:0] sq, next_sq; // next_sq will only be a combinational output, not state.
 always_ff @(posedge reset, posedge clk)
 if (reset == 1) run <= 1; // Asynchronously set run on reset.
 else if (inst == 16'hdf00) run <= 0; // Synchronously clear run on a HLT instruction.
 
 localparam INIT = 0; localparam FETCH = 1;
 localparam FETCH2 =2; localparam LOAD = 3;
 localparam MSIZE8 = 0; localparam MSIZE16 = 1; localparam MSIZE32 = 2;
 localparam REGMUX_ALU = 0; localparam REGMUX_MEM = 1;
 localparam ADDRMUX_ALU = 0; localparam ADDRMUX_IMM16 = 1; localparam ADDRMUX_PC = 2;
 localparam ALUMUX_R2 = 0; localparam ALUMUX_IMM8 = 1; localparam ALUMUX_IMM16 = 2;
 localparam ALU_ADD = 5'd0; localparam ALU_ADC = 5'd1; localparam ALU_SUB = 5'd2;
 localparam ALU_SBC = 5'd3; localparam ALU_NEG = 5'd4; localparam ALU_LSL = 5'd5;
 localparam ALU_ASR = 5'd6; localparam ALU_LSR = 5'd7; localparam ALU_ROR = 5'd8;
 localparam ALU_OR = 5'd9; localparam ALU_AND = 5'd10; localparam ALU_BIC = 5'd11;
 localparam ALU_XOR = 5'd12; localparam ALU_NOT = 5'd13; localparam ALU_SXB = 5'd14;
 localparam ALU_SXW = 5'd15; localparam ALU_ZXB = 5'd16; localparam ALU_ZXW = 5'd17;
 localparam ALU_IN1 = 5'd18; localparam ALU_IN2 = 5'd19;
 always_ff @(posedge reset, posedge clk)
 if (reset == 1) sq <= INIT; // On reset, go to the INIT state
 else if (run == 1) sq <= next_sq; // As long as computer is running, advance to next state
 // Choose the next state
 
 always_comb
 case (sq)
 INIT: next_sq = FETCH;
 FETCH: next_sq = (LDL || STL) ? FETCH2 : FETCH;
 FETCH2: next_sq = STL ? INIT : LOAD;
 LOAD: next_sq = FETCH;
 default: next_sq = INIT;
 endcase
 // A few convenient symbols to refer to the current state.
 wire init = sq == INIT;
 wire fetch = sq == FETCH;
 wire fetch2 = sq == FETCH2;
 wire load = sq == LOAD;
 
 // instruction decoding
 wire INST1R = (opcode & 4'b1000) == 0; // matches all immediate arith/logical instructions
 wire ORI = opcode == 0;
 wire ANDI = opcode == 1;
 wire BICI = opcode == 2;
 wire ADDI = opcode == 3;
 wire SUBI = opcode == 4;
 wire CMPI = opcode == 5;
 wire LIDBU = opcode == 6;
 wire INST2R = (opcode == 8 || opcode == 9); // matches all two-register arith/logical instructions
 wire CMP = opcode == 8 && type == 0;
 wire CPY = opcode == 8 && type == 1;
 wire ADD = opcode == 8 && type == 2;
 wire ADC = opcode == 8 && type == 3;
 wire SUB = opcode == 8 && type == 4;
 wire SBC = opcode == 8 && type == 5;
 wire NEG = opcode == 8 && type == 6;
 wire TST = opcode == 9 && type == 0;
 wire AND = opcode == 9 && type == 1;
 wire OR = opcode == 9 && type == 2;
 wire BIC = opcode == 9 && type == 3;
 wire XOR = opcode == 9 && type == 4;
 wire NOT = opcode == 9 && type == 5;
 wire LDL = opcode == 4'ha;
 wire STL = opcode == 4'hc;
 
  // all the outputs
 assign irl = run & fetch;
 assign iml = run & fetch2;
 assign pcc = run & (next_sq == FETCH || next_sq == FETCH2);
 assign rwe = run & (fetch & ((INST1R & ~CMPI) || (INST2R & ~CMP & ~TST)) || load);
 assign rsel1 = inst[11:8];
 assign rsel2 = inst[3:0];
 assign wsel = rsel1;
 reg [4:0] aluop; // Determined on next page
 reg flagup; // Determined on next page
 assign aop = aluop;
 assign fue = run & fetch & flagup;
 assign regmuxsel = LDL ? REGMUX_MEM: REGMUX_ALU;
 assign alumuxsel = (opcode == 8 || opcode == 9) ? ALUMUX_R2 : ALUMUX_IMM8;
 assign addrmuxsel = fetch2? ADDRMUX_IMM16 : ADDRMUX_PC;
 assign msize = fetch2 ? MSIZE32: MSIZE16;
 assign mwe = fetch2 & STL;
 
  // instruction to ALU operation translation
 always_comb
 case(opcode)
 0: begin aluop = ALU_OR; flagup = 1; end // ORI
 1: begin aluop = ALU_AND; flagup = 1; end // ANDI
 2: begin aluop = ALU_BIC; flagup = 1; end // BICI
 3: begin aluop = ALU_ADD; flagup = 1; end // ADDI
 4: begin aluop = ALU_SUB; flagup = 1; end // SUBI
 5: begin aluop = ALU_SUB; flagup = 1; end // CMPI
 6: begin aluop = ALU_ZXB; flagup = 1; end // LDIBU
 8: case(type)
 0: begin aluop = ALU_SUB; flagup = 1; end // CMP
 1: begin aluop = ALU_IN2; flagup = 1; end // CPY
 2: begin aluop = ALU_ADD; flagup = 1; end // ADD
 3: begin aluop = ALU_ADC; flagup = 1; end // ADC
 4: begin aluop = ALU_SUB; flagup = 1; end // SUB
 5: begin aluop = ALU_SBC; flagup = 1; end // SBC
 6: begin aluop = ALU_NEG; flagup = 1; end // NEG
 default: begin aluop = ALU_IN2; flagup = 0; end
 endcase
 9: case(type)
 0: begin aluop = ALU_AND; flagup = 1; end // TST
 1: begin aluop = ALU_AND; flagup = 1; end // AND
 2: begin aluop = ALU_OR ; flagup = 1; end // OR
 3: begin aluop = ALU_BIC; flagup = 1; end // BIC
 4: begin aluop = ALU_XOR; flagup = 1; end // XOR
 5: begin aluop = ALU_NOT; flagup = 1; end // NOT
 default: begin aluop = ALU_IN2; flagup = 0; end
 endcase
 4'hc: begin aluop = ALU_IN1; flagup = 0; end
 default: begin aluop = ALU_IN2; flagup = 0; end
 endcase
endmodule
