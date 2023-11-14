import java.lang.annotation.*;

public class Annotated {

  @Retention(RetentionPolicy.CLASS)
  @Target(value=ElementType.TYPE_USE)
  public static @interface Ann {}

  public void test () { 
    @Ann Integer a = 0;
  }
}
