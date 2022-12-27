
with Maths;

with Neural_Maths;

package body Fully_Connected_Layer is

   function Backward
     (Layer : Activation_Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Matrix is
      use Neural_Maths;
      Result : Real_Float_Matrix (Layer.Input_Data'Range, Layer.Input_Data'Range (2));
   begin
      for row in Layer.Input_Data'Range loop
         for col in Layer.Input_Data'Range (2) loop
            Result (row, col) :=
              D_Sigmoid (Layer.Input_Data (row, col)) * Out_Error (row);
         end loop;
      end loop;

      return Result;

   end Backward;

   --  --------------------------------------------------------------

   function Backward (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
                      return Real_Float_Vector is
      use Real_Float_Arrays;
      In_Error      : constant Real_Float_Vector :=
                        Out_Error * Transpose (Layer.Weights);
      Weights_Error : constant Real_Float_Vector :=
                        Transpose (Layer.Input_Data) * Out_Error;
   begin
      Layer.Delta_W := Layer.Delta_W + Weights_Error;
      Layer.Bias := Layer.Bias + Out_Error;
      Layer.Passes := Layer.Passes + 1;

      return In_Error;

   end Backward;

   --  --------------------------------------------------------------

   function Forward
     (Layer : out Activation_Layer_Data; Input_Data : Real_Float_Matrix)
      return Real_Float_Matrix is
      Result : Real_Float_Vector (Input_Data'Range);
   begin
      Layer.Input_Data := Input_Data;
      for index in Input_Data'Range loop
         Result (index) := Neural_Maths.Sigmoid (Layer.Input_Data (index));
      end loop;

      return Result;

   end Forward;

   --  --------------------------------------------------------------

   function Forward (Layer : out Layer_Data; Input_Data : Real_Float_Matrix)
                     return Real_Float_Matrix is
      use Real_Float_Arrays;
   begin
      Layer.Input_Data := Input_Data;
      return Input_Data * Layer.Weights + Layer.Bias;

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize (Layer : out Layer_Data) is
   begin
      for row in Layer.Weights'Range loop
         for col in Layer.Weights'Range (2) loop
            Layer.Weights (row, col) := 0.5 * Maths.Random_Float;
         end loop;
         Layer.Bias (row) := 0.5 * Maths.Random_Float;
      end loop;

   end Initialize;

   --  --------------------------------------------------------------

   procedure Step (Layer : Activation_Layer_Data) is
   begin
      null;

   end Step;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      use Real_Float_Arrays;
   begin
      Layer.Weights :=
        Layer.Weights - Eta / Float (Layer.Passes) * Layer.Delta_W;
      Layer.Bias :=
        Layer.Bias - Eta * Layer.Delta_B / Float (Layer.Passes);
      Layer.Delta_W :=
        Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
      Layer.Delta_B := Zero_Array (Layer.Delta_B'Length);
      Layer.Passes := 0;

   end Step;

   --  --------------------------------------------------------------

end Fully_Connected_Layer;
