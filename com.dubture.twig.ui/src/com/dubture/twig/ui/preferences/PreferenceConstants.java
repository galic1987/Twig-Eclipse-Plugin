/*******************************************************************************
 * This file is part of the Twig eclipse plugin.
 * 
 * (c) Robert Gruendler <r.gruendler@gmail.com>
 * 
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 ******************************************************************************/
package com.dubture.twig.ui.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import org.eclipse.ui.internal.editors.text.EditorsPlugin;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.wst.sse.ui.internal.preferences.ui.ColorHelper;

import com.dubture.twig.core.TwigCoreConstants;
import com.dubture.twig.ui.TwigUICorePlugin;
import com.dubture.twig.ui.editor.SemanticHighlightingManager;

/**
 * 
 * 
 */
@SuppressWarnings("restriction")
public class PreferenceConstants
{

    /**
     * A named preference that controls the behavior when double clicking on a
     * container in the folders view.
     * <p>
     * Value is of type <code>String</code>: possible values are <code>
     * DOUBLE_CLICK_GOES_INTO</code> or <code>
     * DOUBLE_CLICK_EXPANDS</code>.
     * </p>
     * 
     * @see #DOUBLE_CLICK_EXPANDS
     * @see #DOUBLE_CLICK_GOES_INTO
     */
    public static final String DOUBLE_CLICK = "phpviewDoubleclick"; //$NON-NLS-1$

    /**
     * A string value used by the named preference <code>DOUBLE_CLICK</code>.
     * 
     * @see #DOUBLE_CLICK
     */
    public static final String DOUBLE_CLICK_EXPANDS = "phpviewDoubleclickExpands"; //$NON-NLS-1$

    /**
     * A string value used by the named preference <code>DOUBLE_CLICK</code>.
     * 
     * @see #DOUBLE_CLICK
     */
    public static final String DOUBLE_CLICK_GOES_INTO = "phpviewGointo"; //$NON-NLS-1$

    /**
     * A named preference that holds the color for the PHP boundary makers
     * (open/close tags)
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_BOUNDARYMARKER_COLOR = "editorColorBoundarymaker"; //$NON-NLS-1$

    public static final String EDITOR_STMT_BOUNDARYMARKER_COLOR = "editorStmtColorBoundarymaker"; //$NON-NLS-1$
    
    public static final String EDITOR_BLOCKNAME_COLOR = "editorBlocknameColor"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHP boundary
     * makers (open/close tags)
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_BOUNDARYMARKER_DEFAULT_COLOR = ColorHelper
            .getColorString(137, 102, 18);

    public static final String EDITOR_STMT_BOUNDARYMARKER_DEFAULT_COLOR = ColorHelper
            .getColorString(137, 102, 18);

    /**
     * A named preference that holds the color for the PHP comments
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_COMMENT_COLOR = "editorColorComment"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHP comments
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_COMMENT_DEFAULT_COLOR = ColorHelper
            .getColorString(128, 128, 128);

    /**
     * A named preference that holds the color for the TASK tag inside the
     * comment
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_TASK_COLOR = "editorColorTask"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the TASK tag inside
     * the comment
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_TASK_DEFAULT_COLOR = ColorHelper
            .getColorString(124, 165, 213) + " | | true"; //$NON-NLS-1$

    /**
     * A named preference that holds the color for the heredoc
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_DOUBLE_QUOTED_COLOR = "editorColorHeredoc"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the heredoc
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_DOUBLE_QUOTED_DEFAULT_COLOR = ColorHelper
            .getColorString(0, 130, 130);

    public static final String EDITOR_HASH_DEFAULT_COLOR = ColorHelper
            .getColorString(0, 130, 130);

    public static final String EDITOR_KEYWORD_DEFAULT_COLOR = ColorHelper
            .packStylePreferences(new String[]{
                    ColorHelper.getColorString(127, 0, 85), null, "true"});
    

    public static final String EDITOR_BLOCKNAME_DEFAULT_COLOR = ColorHelper
            .packStylePreferences(new String[]{
                    ColorHelper.getColorString(127, 0, 85), null, "true"});
    

    /**
     * A named preference that holds the color for the PHP keyword
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_LABEL_COLOR = "editorColorKeyword"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHP keyword
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_LABEL_DEFAULT_COLOR = ColorHelper
            .getColorString(0, 0, 255);

    /**
     * A named preference that holds the color for the normal PHP text
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_NORMAL_COLOR = "codeStyleNormal"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the normal PHP text
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_NORMAL_DEFAULT_COLOR = ColorHelper
            .getColorString(0, 0, 0);

    /**
     * A named preference that holds the color for the numbers
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_NUMBER_COLOR = "editorColorNumber"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the numbers
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_NUMBER_DEFAULT_COLOR = ColorHelper
            .getColorString(255, 0, 0);

    /**
     * A named preference that holds the color for the PHPDoc comments
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_PHPDOC_COLOR = "editorColorPhpdoc"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHPDoc comments
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_PHPDOC_DEFAULT_COLOR = ColorHelper
            .getColorString(128, 128, 128) + " | | true"; //$NON-NLS-1$

    /**
     * A named preference that holds the color for the PHP string
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_STRING_COLOR = "editorColorString"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHP string
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_STRING_DEFAULT_COLOR = ColorHelper
            .getColorString(0, 0, 192);

    /**
     * A named preference that holds the color for the PHP variable
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_VARIABLE_COLOR = "editorColorVariable"; //$NON-NLS-1$

    /**
     * A named preference that holds the default color for the PHP variable
     * <p>
     * Value is of type <code>String</code>.
     * </p>
     */
    public static final String EDITOR_VARIABLE_DEFAULT_COLOR = ColorHelper
            .getColorString(102, 0, 0);

    /**
     * A named preference that defines the key for the hover modifiers.
     */
    public static final String EDITOR_TEXT_HOVER_MODIFIERS = TwigUICorePlugin.PLUGIN_ID
            + "hoverModifiers"; //$NON-NLS-1$

    public static final String TEMPLATES_KEY = "org.eclipse.php.smarty.ui.editor.templates"; //$NON-NLS-1$	

    public static final String AUTOCLOSE_PRINT_TAGS = "autoclose_print_tags";

    public static final String AUTOCLOSE_STATEMENT_TAGS = "autoclose_statement_tags";

    public static final String AUTOCREATE_STATEMENT_TAGS = "autocreate_statement_tags";

    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_PREFIX = "semanticHighlighting."; //$NON-NLS-1$
    
    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_BOLD_SUFFIX = ".bold"; //$NON-NLS-1$
    
    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_COLOR_SUFFIX = ".color"; //$NON-NLS-1$

    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_BGCOLOR_SUFFIX = ".bgcolor"; //$NON-NLS-1$

    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_ITALIC_SUFFIX = ".italic"; //$NON-NLS-1$
    
    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_STRIKETHROUGH_SUFFIX = ".strikethrough"; //$NON-NLS-1$
    
    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_UNDERLINE_SUFFIX = ".underline"; //$NON-NLS-1$
    
    public static final String EDITOR_SEMANTIC_HIGHLIGHTING_ENABLED_SUFFIX = ".enabled"; //$NON-NLS-1$	

    public static final String EDITOR_KEYWORD_COLOR = "editorColorKeyword"; //$NON-NLS-1$

    public static final String EDITOR_LINE_COMMENT_COLOR = "editorColorLineComment"; //$NON-NLS-1$	

    public static final String EDITOR_PHPDOC_COMMENT_COLOR = "editorColorPHPDocComment"; //$NON-NLS-1$	

    public static final String EDITOR_HEREDOC_COLOR = "editorColorHeredoc"; //$NON-NLS-1$

    public static final String EDITOR_HASH_COLOR = "editorColorJson";
    
    public static final String EDITOR_INTERPOLATION_COLOR= "editorColorInterpolation";

    /**
     * A named preference that controls whether the 'close braces' feature is
     * enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_CLOSE_BRACES = "closeBraces"; //$NON-NLS-1$

    /**
     * A named preference that controls whether the 'close brackets' feature is
     * enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_CLOSE_BRACKETS = "closeBrackets"; //$NON-NLS-1$

    /**
     * A named preference that controls whether the 'close strings' feature is
     * enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_CLOSE_STRINGS = "closeStrings"; //$NON-NLS-1$

    /**
     * A named preference that controls whether the 'close phpdoc and comments'
     * feature is enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_CLOSE_PHPDOCS_AND_COMMENTS = "closePhpDocsAndComments"; //$NON-NLS-1$

    /**
     * A named preference that controls whether the 'close php close tag'
     * feature is enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_ADD_PHPCLOSE_TAGS = "autoAddPhpCloseTags"; //$NON-NLS-1$

    /**
     * A named preference that controls whether the 'Add "php" after PHP start
     * tag (<?)' feature is enabled.
     * <p>
     * Value is of type <code>Boolean</code>.
     * </p>
     */
    public final static String EDITOR_ADD_PHP_FOR_PHPSTART_TAGS = "autoAddPhpForPhpStartTags"; //$NON-NLS-1$

    public static void initializeDefaultValues()
    {

        // Override Editor Preference defaults:
        IPreferenceStore editorStore = EditorsPlugin.getDefault()
                .getPreferenceStore();

        // Show current line:
        editorStore
                .setDefault(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE,
                        true);

        // Show line numbers:
        editorStore
                .setDefault(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER,
                        true);

        IPreferenceStore store = getPreferenceStore();

        // SyntaxColoringPage
        store.setDefault(EDITOR_NORMAL_COLOR, EDITOR_NORMAL_DEFAULT_COLOR);
        store.setDefault(EDITOR_BOUNDARYMARKER_COLOR, EDITOR_BOUNDARYMARKER_DEFAULT_COLOR);
        store.setDefault(EDITOR_STMT_BOUNDARYMARKER_COLOR, EDITOR_STMT_BOUNDARYMARKER_DEFAULT_COLOR);
        store.setDefault(EDITOR_LABEL_COLOR, EDITOR_LABEL_DEFAULT_COLOR);
        store.setDefault(EDITOR_VARIABLE_COLOR, EDITOR_VARIABLE_DEFAULT_COLOR);
        store.setDefault(EDITOR_STRING_COLOR, EDITOR_STRING_DEFAULT_COLOR);
        store.setDefault(EDITOR_HASH_COLOR, EDITOR_HASH_DEFAULT_COLOR);
        store.setDefault(EDITOR_INTERPOLATION_COLOR, EDITOR_STMT_BOUNDARYMARKER_DEFAULT_COLOR);
        store.setDefault(EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_DEFAULT_COLOR);
        store.setDefault(EDITOR_BLOCKNAME_COLOR, EDITOR_BLOCKNAME_DEFAULT_COLOR);        
        store.setDefault(EDITOR_NUMBER_COLOR, EDITOR_NUMBER_DEFAULT_COLOR);
        store.setDefault(EDITOR_DOUBLE_QUOTED_COLOR, EDITOR_DOUBLE_QUOTED_DEFAULT_COLOR);
        store.setDefault(EDITOR_COMMENT_COLOR, EDITOR_COMMENT_DEFAULT_COLOR);

        store.setDefault(getEnabledPreferenceKey(EDITOR_NORMAL_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_BOUNDARYMARKER_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_STMT_BOUNDARYMARKER_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_LABEL_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_VARIABLE_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_STRING_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_HASH_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_INTERPOLATION_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_KEYWORD_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_BLOCKNAME_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_NUMBER_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_DOUBLE_QUOTED_COLOR), true);
        store.setDefault(getEnabledPreferenceKey(EDITOR_COMMENT_COLOR), true);
        
        store.setDefault(TwigCoreConstants.SYNTAX_PROBLEM_SEVERITY, TwigCoreConstants.SYNTAX_IGNORE);
        
        // code assist
        store.setDefault(AUTOCLOSE_PRINT_TAGS, true);
        store.setDefault(AUTOCLOSE_STATEMENT_TAGS, true);
        
        SemanticHighlightingManager.getInstance().initDefaults(store);

    }

    public static String getEnabledPreferenceKey(String preferenceKey)
    {
        String key = PreferenceConstants.EDITOR_SEMANTIC_HIGHLIGHTING_PREFIX
                + preferenceKey
                + PreferenceConstants.EDITOR_SEMANTIC_HIGHLIGHTING_ENABLED_SUFFIX;
        return key;
    }

    public static IPreferenceStore getPreferenceStore()
    {
        return TwigUICorePlugin.getDefault().getPreferenceStore();
    }

}
