
with PLplot_Auxiliary; use PLplot_Auxiliary;

with NL_Types;

package Plotting is
   
   procedure Display_Image (Data : NL_Types.Float_List);
   procedure Plot (Bitmap : Real_Matrix);
   
end Plotting;
