
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Neural_Processes is

   type Layer_Type is (Activation_Layer, Hidden_Layer);

   --     type Activation_Layer_Data (Num_Samples, Input_Size : Positive) is record
   --        Input_Data : Real_Float_Matrix (1 .. Num_Samples, 1 .. Input_Size);
   --     end record;

   type Layer_Data (Layer_Kind  : Layer_Type; Input_Size : Natural;
                    Output_Size : Natural) is
      record
         case Layer_Kind is
         when Activation_Layer => null;
         when Hidden_Layer =>
            Data : Real_Float_Matrix (1 .. Input_Size, 1 .. Output_Size);
            Weights    : Real_Float_Matrix (1 .. Input_Size, 1 .. Output_Size);
            Bias       : Real_Float_Matrix (1 .. 1, 1 .. Output_Size);
            Delta_W    : Real_Float_Matrix (1 .. Input_Size, 1 .. Output_Size) :=
                           (others => (others => 0.0));
            Delta_B    : Real_Float_Matrix (1 .. 1, 1 .. Output_Size) :=
                           (others => (others => 0.0));
            Passes     : Natural := 0;
         end case;
      end record;

   function Backward
     (Layer : Layer_Data; Out_Error : Real_Float_Matrix)
      return Real_Float_Matrix;
   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector;
   function Forward (Layer : out Layer_Data; Input_Data : Real_Float_Matrix)
                     return Real_Float_Matrix;
   procedure Initialize (Layer : out Layer_Data);
   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix;
   function Load_Data (File_Name : String) return Real_Float_Vector;
   function Loss (Y_True, Y_Pred : Real_Float_Matrix) return Float;
   function Loss_Deriv (Y_True, Y_Pred : Real_Float_Matrix)
                        return Real_Float_Matrix;
   procedure Step (Layer : Layer_Data);
   procedure Step (Layer : in out Layer_Data; Eta : Float);

end Neural_Processes;
