
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Word_Classification is

   function Get_Features (aLine : Unbounded_String)
                          return ML_Types.Unbounded_List;

end Word_Classification;
