
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Dense is
   type Layer_Type is (Tanh_Layer, Dense_Layer);
   type Layer_Range is new Integer range 0 .. Integer'Last / 2;
   type Layer_Matrix is
     array (Layer_Range range <>, Layer_Range range <>) of Float;
   type Layer_Vector is array (Layer_Range range <>) of Float;

   type Layer_Data (Layer_Kind              : Layer_Type := Tanh_Layer;
                    Input_Size, Output_Size : Layer_Range := 0) is
      record
         Input_Data : Layer_Matrix (1 .. 1, 1 .. Input_Size);
         case Layer_Kind is
            when Dense_Layer =>
               Weights : Layer_Matrix (1 .. Output_Size, 1 .. Input_Size);
               Bias    : Layer_Matrix (1 .. Output_Size, 1 .. 1);
               Delta_W : Layer_Matrix (1 .. Input_Size, 1 .. Output_Size) :=
                           (others => (others => 0.0));
               Delta_B : Layer_Matrix (1 .. Output_Size, 1 .. 1) :=
                           (others =>  (others => 0.0));
               Passes  : Natural := 0;
            when Tanh_Layer =>
               null;
         end case;
      end record;

   procedure Backward
     (Layer        : in out Layer_Data;
      Out_Gradient : in out Real_Float_List; Learning_Rate : Float);
   procedure Forward (Layer : in out Layer_Data;
                      Data  : in out Real_Float_List);

end Dense;
