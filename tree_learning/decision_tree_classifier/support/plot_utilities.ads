
with PLplot_Auxiliary;

with ML_Types;

package Plot_Utilities is

   function To_PL_Array (List_1D  : ML_Types.Value_Data_List;
                         Num_Rows : Positive)
                          return PLplot_Auxiliary.Real_Matrix;

end Plot_Utilities;
