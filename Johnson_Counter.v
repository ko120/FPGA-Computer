module top (hz100,reset,pb,ss7,ss6,ss5,ss4,ss3,ss2,ss1,ss0,left,right,red,green,blue);
  input hz100, reset;
  input [20:0] pb;
  output [7:0] ss7, ss6, ss5, ss4, ss3, ss2, ss1, ss0, left, right;
  output red, green, blue;

 //STEP 6 LEFT
 jonson x (.C(pb[19]),.R(reset),.Q(left[7:0]));
 
 //STEP3
 //SB_DFFR ff1 (.Q(right[0]), .C(pb[19]), .R(reset), .D(pb[0]));

  //assign left[0] = pb[0];
  
  //STEP 4
  /*
  SB_DFFR ff0 (.Q(right[0]), .C(pb[19]), .R(reset), .D(pb[0]));
  SB_DFFR ff1 (.Q(right[1]), .C(pb[19]), .R(reset), .D(pb[1]));
  SB_DFFR ff2 (.Q(right[2]), .C(pb[19]), .R(reset), .D(pb[2]));
  SB_DFFR ff3 (.Q(right[3]), .C(pb[19]), .R(reset), .D(pb[3]));
  SB_DFFR ff4 (.Q(right[4]), .C(pb[19]), .R(reset), .D(pb[4]));
  SB_DFFR ff5 (.Q(right[5]), .C(pb[19]), .R(reset), .D(pb[5]));
  SB_DFFR ff6 (.Q(right[6]), .C(pb[19]), .R(reset), .D(pb[6]));
  SB_DFFR ff7 (.Q(right[7]), .C(pb[19]), .R(reset), .D(pb[7]));
  */
  
  //STEP 5 JOHNSON Right
  
  SB_DFFR ff0 (.Q(right[0]), .C(pb[19]), .R(reset), .D(~right[7]));
  SB_DFFR ff1 (.Q(right[1]), .C(pb[19]), .R(reset), .D(right[0]));
  SB_DFFR ff2 (.Q(right[2]), .C(pb[19]), .R(reset), .D(right[1]));
  SB_DFFR ff3 (.Q(right[3]), .C(pb[19]), .R(reset), .D(right[2]));
  SB_DFFR ff4 (.Q(right[4]), .C(pb[19]), .R(reset), .D(right[3]));
  SB_DFFR ff5 (.Q(right[5]), .C(pb[19]), .R(reset), .D(right[4]));
  SB_DFFR ff6 (.Q(right[6]), .C(pb[19]), .R(reset), .D(right[5]));
  SB_DFFR ff7 (.Q(right[7]), .C(pb[19]), .R(reset), .D(right[6]));

endmodule

// Add more modules down here...

module jonson(C,R,Q);
  input C,R;
  output reg [7:0] Q; 
  
  always @(posedge C, posedge R)
  begin
  if (R)
      Q <= 8'b0; 
  else 
      Q <= {Q[6:0], ~Q[7]};
      
   end
   
   endmodule
