
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Fully_Connected_Layer is

   type Activation_Layer_Data (Input_Size : Positive) is record
      Input_Data : Real_Float_Matrix (1 .. Input_Size, 1 .. 14);
   end record;

   type Layer_Data (Input_Size, Output_Size : Positive) is record
      Weights    : Real_Float_Matrix (1 .. Input_Size, 1 .. Output_Size);
      Bias       : Real_Float_Vector (1 .. Output_Size);
      Delta_W    : Real_Float_Matrix (1 .. Input_Size, 1 .. Output_Size) :=
                     (others => (others => 0.0));
      Delta_B    : Real_Float_Vector (1 .. Output_Size) := (others => 0.0);
      Input_Data : Real_Float_Matrix (1 .. Input_Size, 1 .. 14);
      Passes     : Natural := 0;
   end record;

   function Backward
     (Layer : Activation_Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector;
   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector;
   function Forward
     (Layer : out Activation_Layer_Data; Input_Data : Real_Float_Matrix)
      return Real_Float_Matrix;
   function Forward (Layer : out Layer_Data; Input_Data : Real_Float_Matrix)
                     return Real_Float_Matrix;
   procedure Initialize (Layer : out Layer_Data);
   procedure Step (Layer : Activation_Layer_Data);
   procedure Step (Layer : in out Layer_Data; Eta : Float);

end Fully_Connected_Layer;