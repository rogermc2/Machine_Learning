
with Maths;

package body Fully_Connected_Layer is

   function Forward (Layer : out Layer_Data; Input_Data :  Real_Float_Matrix)
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

end Fully_Connected_Layer;
