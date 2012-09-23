package views

import views.html.helper._

package object support {
  /**
   * Twitter bootstrap input structure.
   *
   * {{{
   * <dl>
   *   <dt><label for="username"></dt>
   *   <dd><input type="text" name="username" id="username"></dd>
   *   <dd class="error">This field is required!</dd>
   * </dl>
   * }}}
   */
  implicit val twitterBootstrapField = new FieldConstructor {
    def apply(elts: FieldElements) = views.html.support.fieldConstructor(elts)
  }
}
