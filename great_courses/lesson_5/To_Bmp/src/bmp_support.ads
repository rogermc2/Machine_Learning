
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BMP_Support is

   procedure Process (name : String; as_background, test_only : Boolean;
                     background_image_name : in out Unbounded_String);

end BMP_Support;
