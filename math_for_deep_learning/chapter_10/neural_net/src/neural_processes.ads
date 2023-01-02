
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Neural_Processes is
   type Layer_Type is (Activation_Layer, Hidden_Layer);
   type Layer_Range is new Integer range 0 .. Integer'Last / 2;
   type Layer_Matrix is
     array (Layer_Range range <>, Layer_Range range <>) of Float;
   type Layer_Vector is array (Layer_Range range <>) of Float;

   type Layer_Data (Layer_Kind              : Layer_Type := Activation_Layer;
                    Input_Size, Output_Size : Layer_Range := 0) is
      record
         Input_Data : Layer_Vector (1 .. Input_Size);
         case Layer_Kind is
            when Hidden_Layer =>
               Weights : Layer_Matrix (1 .. Input_Size, 1 .. Output_Size);
               Bias    : Layer_Vector (1 .. Output_Size);
               Delta_W : Layer_Matrix (1 .. Input_Size, 1 .. Output_Size) :=
                           (others => (others => 0.0));
               Delta_B : Layer_Vector (1 .. Output_Size) := (others => 0.0);
               Passes  : Natural := 0;
            when Activation_Layer =>
               null;
         end case;
      end record;

   procedure Backward
     (Layer : in out Layer_Data; Out_Error : in out Real_Float_List);
   procedure Forward (Layer : in out Layer_Data; Data : in out Real_Float_List);
   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Layer_Range);
   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix;
   function Load_Data (File_Name : String) return Real_Float_Vector;
   function Mean_Square_Error (Y_True,Y_Pred : Real_Float_Vector)
                               return Float;
   function MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                            return Real_Float_Vector;
   procedure Step (Layer : in out Layer_Data; Eta : Float);

end Neural_Processes;
