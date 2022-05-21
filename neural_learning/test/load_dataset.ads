
with NL_Types;

package Load_Dataset is

   procedure Load_Digits (Features, Target : out NL_Types.Value_Data_Lists_2D);
   procedure Load_Iris (Features, Target : out NL_Types.Value_Data_Lists_2D);

end Load_Dataset;
