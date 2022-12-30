
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Neural_Processes is

   type Layer_Type is (Activation_Layer, Hidden_Layer);

   --     type Activation_Layer_Data (Num_Samples, Input_Size : Positive) is record
   --        Input_Data : Real_Float_Matrix (1 .. Num_Samples, 1 .. Input_Size);
   --     end record;
   type Layer_Size is new Natural range 0 .. Integer'Last;

   type Layer_Data (Layer_Kind              : Layer_Type;
                    Input_Size, Output_Size : Natural) is
      record
         Input_Data : Real_Float_Vector (1 .. Input_Size);
         case Layer_Kind is
         when Activation_Layer => null;
            --           when Hidden_Layer =>
            --              Weights : Real_Float_List_2D;
            --              Bias    : Real_Float_List;
            --              Delta_W : Real_Float_List_2D;
            --              Delta_B : Real_Float_List;
            --              Passes  : Natural := 0;
            when Hidden_Layer =>
               Weights           : Real_Float_Matrix (1 .. Input_Size,
                                                      1 .. Output_Size);
               Bias              : Real_Float_Vector (1 .. Output_Size);
               Delta_W           : Real_Float_Matrix (1 .. Input_Size,
                                                      1 .. Output_Size) :=
                                     (others => (others => 0.0));
               Delta_B           : Real_Float_Vector (1 .. Output_Size) :=
                                     (others => 0.0);
               Passes            : Natural := 0;
         end case;
      end record;

   --     function Backward
   --       (Layer : Layer_Data; Out_Error : Real_Float_Matrix)
   --        return Real_Float_Matrix;
   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector;
   function Forward (Layer : in out Layer_Data; Input_Data : Real_Float_List)
                     return Real_Float_List;
   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Positive);
   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix;
   function Load_Data (File_Name : String) return Real_Float_Vector;
   function Loss (Y_True,Y_Pred : Real_Float_Vector) return Float;
   function Loss_Deriv (Y_True, Y_Pred : Real_Float_Vector)
                        return Real_Float_Vector;
   procedure Step (Layer : Layer_Data);
   procedure Step (Layer : in out Layer_Data; Eta : Float);

end Neural_Processes;
