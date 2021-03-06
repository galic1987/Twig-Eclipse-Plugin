/*******************************************************************************
 * This file is part of the Twig eclipse plugin.
 * 
 * (c) Robert Gruendler <r.gruendler@gmail.com>
 * 
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 ******************************************************************************/
package com.dubture.twig.ui.editor.folding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.dltk.compiler.problem.DefaultProblem;
import org.eclipse.dltk.core.DLTKCore;
import org.eclipse.dltk.core.ElementChangedEvent;
import org.eclipse.dltk.core.IElementChangedListener;
import org.eclipse.dltk.core.IMember;
import org.eclipse.dltk.core.IModelElement;
import org.eclipse.dltk.core.IModelElementDelta;
import org.eclipse.dltk.core.IParent;
import org.eclipse.dltk.core.ISourceRange;
import org.eclipse.dltk.core.ISourceReference;
import org.eclipse.dltk.core.ModelException;
import org.eclipse.dltk.corext.SourceRange;
import org.eclipse.dltk.internal.ui.editor.EditorUtility;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.IProjectionListener;
import org.eclipse.jface.text.source.projection.IProjectionPosition;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.php.internal.ui.PHPUiPlugin;
import org.eclipse.php.internal.ui.editor.PHPStructuredEditor;
import org.eclipse.php.internal.ui.editor.PHPStructuredTextViewer;
import org.eclipse.php.internal.ui.editor.configuration.PHPStructuredTextViewerConfiguration;
import org.eclipse.php.internal.ui.folding.IStructuredTextFoldingProvider;
import org.eclipse.php.internal.ui.folding.StructuredTextFoldingProviderPHP;
import org.eclipse.php.internal.ui.folding.html.ProjectionModelNodeAdapterFactoryHTML;
import org.eclipse.php.internal.ui.folding.html.ProjectionModelNodeAdapterHTML;
import org.eclipse.php.internal.ui.preferences.PreferenceConstants;
import org.eclipse.php.internal.ui.text.DocumentCharacterIterator;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.PropagatingAdapter;
import org.eclipse.wst.sse.core.internal.model.FactoryRegistry;
import org.eclipse.wst.sse.core.internal.provisional.INodeAdapter;
import org.eclipse.wst.sse.core.internal.provisional.INodeNotifier;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocument;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocumentRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.w3c.dom.Node;

import com.dubture.twig.core.documentModel.parser.TwigSourceElementRequestorExtension;
import com.dubture.twig.core.documentModel.parser.regions.ITwigScriptRegion;
import com.dubture.twig.core.documentModel.parser.regions.TwigRegionTypes;

/**
 * 
 * A folding structured provider based on
 * {@link StructuredTextFoldingProviderPHP} to work with twig.
 * 
 * 
 * Q: How is folding implemented? A: To enable folding for Twig structures like
 * {% block %} {% endblock %}, the {@link TwigSourceElementRequestorExtension}
 * parses twig sources and adds those structures as "PHP Methods" to the php
 * engine.
 * 
 * This way, we can use the built-in PDT folding logic and don't have to provide
 * our own hints on where folding starts and end.
 * 
 * 
 * 
 * 
 * @author Robert Gruendler <r.gruendler@gmail.com>
 * 
 */
@SuppressWarnings("restriction")
public class StructuredTextFoldingProviderTwig implements IProjectionListener,
        IStructuredTextFoldingProvider
{

    private StructuredTextFoldingProviderPHP phpFoldingProvider;
    private ProjectionListener fProjectionListener;
    private PHPStructuredTextViewer viewer;
    private PHPStructuredEditor fEditor;
    private IElementChangedListener fElementListener;
    private volatile int fUpdatingCount = 0;

    private IModelElement fInput;

    /* preferences */
    private boolean fCollapsePhpdoc = true;
    private boolean fCollapseImportContainer = true;
    private boolean fCollapseTypes = true;
    private boolean fCollapseMembers = false;
    private boolean fCollapseHeaderComments = true;

    /**
     * Maximum number of child nodes to add adapters to (limit for performance
     * sake)
     */
    private static final int MAX_CHILDREN = 10;
    /**
     * Maximum number of sibling nodes to add adapters to (limit for performance
     * sake)
     */
    private static final int MAX_SIBLINGS = 1000;

    private IDocument fDocument;

    private interface ICommentScanner
    {

        public abstract void resetTo(int start);

        public abstract int computePreviousComment();

        public abstract int getCurrentCommentStartPosition();

        public abstract int getCurrentCommentEndPosition();

    }

    private static final class TwigElementPosition extends Position implements
            IProjectionPosition
    {

        private IMember fMember;

        public TwigElementPosition(int offset, int length, IMember member)
        {
            super(offset, length);
            Assert.isNotNull(member);
            fMember = member;
        }

        public void setMember(IMember member)
        {
            Assert.isNotNull(member);
            fMember = member;
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeFoldingRegions(org.eclipse.jface.text.IDocument)
         */
        public IRegion[] computeProjectionRegions(IDocument document)
                throws BadLocationException
        {
            int nameStart = offset;
            try {
                /*
                 * The member's name range may not be correct. However,
                 * reconciling would trigger another element delta which would
                 * lead to reentrant situations. Therefore, we optimistically
                 * assume that the name range is correct, but double check the
                 * received lines below.
                 */
                ISourceRange nameRange = fMember.getNameRange();
                if (nameRange != null)
                    nameStart = nameRange.getOffset();

            } catch (ModelException e) {
                // ignore and use default
            }

            int firstLine = document.getLineOfOffset(offset);
            int captionLine = document.getLineOfOffset(nameStart);
            int lastLine = document.getLineOfOffset(offset + length);

            /*
             * see comment above - adjust the caption line to be inside the
             * entire folded region, and rely on later element deltas to correct
             * the name range.
             */
            if (captionLine < firstLine)
                captionLine = firstLine;
            if (captionLine > lastLine)
                captionLine = lastLine;

            IRegion preRegion;
            if (firstLine < captionLine) {
                int preOffset = document.getLineOffset(firstLine);
                IRegion preEndLineInfo = document
                        .getLineInformation(captionLine);
                int preEnd = preEndLineInfo.getOffset();
                preRegion = new Region(preOffset, preEnd - preOffset);
            } else {
                preRegion = null;
            }

            if (captionLine < lastLine) {
                int postOffset = document.getLineOffset(captionLine + 1);
                IRegion postRegion = new Region(postOffset, offset + length
                        - postOffset);

                if (preRegion == null)
                    return new IRegion[]{postRegion};

                return new IRegion[]{preRegion, postRegion};
            }

            if (preRegion != null)
                return new IRegion[]{preRegion};

            return null;
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeCaptionOffset(org.eclipse.jface.text.IDocument)
         */
        public int computeCaptionOffset(IDocument document)
                throws BadLocationException
        {
            int nameStart = offset;
            try {
                // need a reconcile here?
                ISourceRange nameRange = fMember.getNameRange();
                if (nameRange != null)
                    nameStart = nameRange.getOffset();
            } catch (ModelException e) {
                // ignore and use default
            }

            return nameStart - offset;
        }

    }

    private static final class CommentPosition extends Position implements
            IProjectionPosition
    {
        CommentPosition(int offset, int length)
        {
            super(offset, length);
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeFoldingRegions(org.eclipse.jface.text.IDocument)
         */
        public IRegion[] computeProjectionRegions(IDocument document)
                throws BadLocationException
        {
            DocumentCharacterIterator sequence = new DocumentCharacterIterator(
                    document, offset, offset + length);
            int prefixEnd = 0;
            int contentStart = findFirstContent(sequence, prefixEnd);

            int firstLine = document.getLineOfOffset(offset + prefixEnd);
            int captionLine = document.getLineOfOffset(offset + contentStart);
            int lastLine = document.getLineOfOffset(offset + length);

            Assert.isTrue(firstLine <= captionLine,
                    "first folded line is greater than the caption line"); //$NON-NLS-1$
            Assert.isTrue(captionLine <= lastLine,
                    "caption line is greater than the last folded line"); //$NON-NLS-1$

            IRegion preRegion;
            if (firstLine < captionLine) {
                int preOffset = document.getLineOffset(firstLine);
                IRegion preEndLineInfo = document
                        .getLineInformation(captionLine);
                int preEnd = preEndLineInfo.getOffset();
                preRegion = new Region(preOffset, preEnd - preOffset);
            } else {
                preRegion = null;
            }

            if (captionLine < lastLine) {
                int postOffset = document.getLineOffset(captionLine + 1);
                IRegion postRegion = new Region(postOffset, offset + length
                        - postOffset);

                if (preRegion == null)
                    return new IRegion[]{postRegion};

                return new IRegion[]{preRegion, postRegion};
            }

            if (preRegion != null)
                return new IRegion[]{preRegion};

            return null;
        }

        /**
         * Finds the offset of the first identifier part within
         * <code>content</code>. Returns 0 if none is found.
         * 
         * @param content
         *            the content to search
         * @param prefixEnd
         *            the end of the prefix
         * @return the first index of a unicode identifier part, or zero if none
         *         can be found
         */
        private int findFirstContent(final CharSequence content, int prefixEnd)
        {
            int lenght = content.length();
            for (int i = prefixEnd; i < lenght; i++) {
                if (Character.isUnicodeIdentifierPart(content.charAt(i)))
                    return i;
            }
            return 0;
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionPosition#
         * computeCaptionOffset(org.eclipse.jface.text.IDocument)
         */
        public int computeCaptionOffset(IDocument document)
        {
            DocumentCharacterIterator sequence = new DocumentCharacterIterator(
                    document, offset, offset + length);
            return findFirstContent(sequence, 0);
        }
    }

    private static final class Tuple
    {
        TwigProjectionAnnotation annotation;
        Position position;

        Tuple(TwigProjectionAnnotation annotation, Position position)
        {
            this.annotation = annotation;
            this.position = position;
        }
    }

    public class CommentScanner implements ICommentScanner
    {

        private final IDocument document;
        private int startElement;
        private int start;
        private int end;

        public CommentScanner(IDocument document)
        {
            if (document == null) {
                throw new IllegalArgumentException();
            }
            this.document = document;
        }

        public void resetTo(int start)
        {
            this.startElement = start;
        }

        public int getCurrentCommentStartPosition()
        {
            return start;
        }

        public int computePreviousComment()
        {
            start = startElement - 1;

            try {
                // ignore whitespaces
                while (start > 0
                        && Character.isWhitespace(document.getChar(start))) {
                    start--;
                }

                if (start > 0 && document.getChar(start--) != '{') {
                    return startElement;
                }
                // remember the end of the comment
                int end = start;

                if (start > 0 && document.getChar(start--) != '#') {
                    return startElement;
                }

                while (start > 0
                        && (document.getChar(start) != '#' || document
                                .getChar(start - 1) != '}')) {
                    start--;
                }

                if (start == 0)
                    return startElement;
                else {
                    this.end = end;
                    return start;
                }

            } catch (BadLocationException e) {
                return startElement;
            }
        }

        public int getCurrentCommentEndPosition()
        {
            return this.end;
        }
    }

    /**
     * A {@link ProjectionAnnotation} for java code.
     */
    protected static final class TwigProjectionAnnotation extends
            ProjectionAnnotation
    {

        private IModelElement fJavaElement;
        private boolean fIsComment;

        /**
         * Creates a new projection annotation.
         * 
         * @param isCollapsed
         *            <code>true</code> to set the initial state to collapsed,
         *            <code>false</code> to set it to expanded
         * @param element
         *            the java element this annotation refers to
         * @param isComment
         *            <code>true</code> for a foldable comment,
         *            <code>false</code> for a foldable code element
         */
        public TwigProjectionAnnotation(boolean isCollapsed,
                IModelElement element, boolean isComment)
        {
            super(isCollapsed);
            fJavaElement = element;
            fIsComment = isComment;
        }

        IModelElement getElement()
        {
            return fJavaElement;
        }

        void setElement(IModelElement element)
        {
            fJavaElement = element;
        }

        boolean isComment()
        {
            return fIsComment;
        }

        void setIsComment(boolean isComment)
        {
            fIsComment = isComment;
        }

        /*
         * @see java.lang.Object#toString()
         */
        public String toString()
        {
            return "TwigProjectionAnnotation:\n" + //$NON-NLS-1$
                    "\telement: \t" + fJavaElement.toString() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
                    "\tcollapsed: \t" + isCollapsed() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
                    "\tcomment: \t" + isComment() + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /**
     * A context that contains the information needed to compute the folding
     * structure of an {@link ICompilationUnit} or an {@link IClassFile}.
     * Computed folding regions are collected via
     * {@linkplain #addProjectionRange(StructuredTextFoldingProviderPHP.TwigProjectionAnnotation, Position)
     * addProjectionRange}.
     */
    protected final class FoldingStructureComputationContext
    {
        private final ProjectionAnnotationModel fModel;
        private final IDocument fDocument;

        private final boolean fAllowCollapsing;

        private IModelElement fFirstElement;
        private ICommentScanner fScanner;
        private boolean fHasHeaderComment;
        private LinkedHashMap<Object, Position> fMap = new LinkedHashMap<Object, Position>();

        private boolean headerChecked = false;

        private FoldingStructureComputationContext(IDocument document,
                ProjectionAnnotationModel model, boolean allowCollapsing)
        {
            Assert.isNotNull(document);
            Assert.isNotNull(model);
            fDocument = document;
            fModel = model;
            fAllowCollapsing = allowCollapsing;
        }

        private void setFirstElement(IModelElement element)
        {
            if (hasFirstElement())
                throw new IllegalStateException();
            fFirstElement = element;
        }

        boolean hasFirstElement()
        {
            return fFirstElement != null;
        }

        private IModelElement getFirstElement()
        {
            return fFirstElement;
        }

        private boolean hasHeaderComment()
        {
            return fHasHeaderComment;
        }

        private ICommentScanner getScanner()
        {
            if (fScanner == null)
                fScanner = new CommentScanner(fDocument);
            return fScanner;
        }

        private void setHasHeaderComment()
        {
            fHasHeaderComment = true;
        }

        /**
         * Returns <code>true</code> if newly created folding regions may be
         * collapsed, <code>false</code> if not. This is usually
         * <code>false</code> when updating the folding structure while typing;
         * it may be <code>true</code> when computing or restoring the initial
         * folding structure.
         * 
         * @return <code>true</code> if newly created folding regions may be
         *         collapsed, <code>false</code> if not
         */
        public boolean allowCollapsing()
        {
            return fAllowCollapsing;
        }

        /**
         * Returns the document which contains the code being folded.
         * 
         * @return the document which contains the code being folded
         */
        private IDocument getDocument()
        {
            return fDocument;
        }

        private ProjectionAnnotationModel getModel()
        {
            return fModel;
        }

        /**
         * Adds a projection (folding) region to this context. The created
         * annotation / position pair will be added to the
         * {@link ProjectionAnnotationModel} of the {@link ProjectionViewer} of
         * the editor.
         * 
         * @param annotation
         *            the annotation to add
         * @param position
         *            the corresponding position
         */
        public void addProjectionRange(TwigProjectionAnnotation annotation,
                Position position)
        {
            fMap.put(annotation, position);
        }

        /**
         * Returns <code>true</code> if header comments should be collapsed.
         * 
         * @return <code>true</code> if header comments should be collapsed
         */
        public boolean collapseHeaderComments()
        {
            return fAllowCollapsing && fCollapseHeaderComments;
        }

        /**
         * Returns <code>true</code> if import containers should be collapsed.
         * 
         * @return <code>true</code> if import containers should be collapsed
         */
        public boolean collapseImportContainer()
        {
            return fAllowCollapsing && fCollapseImportContainer;
        }

        /**
         * Returns <code>true</code> if inner types should be collapsed.
         * 
         * @return <code>true</code> if inner types should be collapsed
         */
        public boolean collapseTypes()
        {
            return fAllowCollapsing && fCollapseTypes;
        }

        /**
         * Returns <code>true</code> if javadoc comments should be collapsed.
         * 
         * @return <code>true</code> if javadoc comments should be collapsed
         */
        public boolean collapseJavadoc()
        {
            return fAllowCollapsing && fCollapsePhpdoc;
        }

        /**
         * Returns <code>true</code> if methods should be collapsed.
         * 
         * @return <code>true</code> if methods should be collapsed
         */
        public boolean collapseMembers()
        {
            return fAllowCollapsing && fCollapseMembers;
        }

        /**
         * @return true if the header was computed already
         */
        public boolean isHeaderChecked()
        {
            return headerChecked;
        }

        /**
         * set the header checked property
         */
        public void setHeaderChecked()
        {
            headerChecked = true;
        }

    }

    /**
     * Internal projection listener.
     */
    private final class ProjectionListener implements IProjectionListener
    {
        private ProjectionViewer fViewer;

        /**
         * Registers the listener with the viewer.
         * 
         * @param viewer
         *            the viewer to register a listener with
         */
        public ProjectionListener(ProjectionViewer viewer)
        {
            Assert.isLegal(viewer != null);
            fViewer = viewer;
            fViewer.addProjectionListener(this);
        }

        /**
         * Disposes of this listener and removes the projection listener from
         * the viewer.
         */
        public void dispose()
        {
            if (fViewer != null) {
                fViewer.removeProjectionListener(this);
                fViewer = null;
            }
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionListener#
         * projectionEnabled()
         */
        public void projectionEnabled()
        {
            handleProjectionEnabled();
        }

        /*
         * @seeorg.eclipse.jface.text.source.projection.IProjectionListener#
         * projectionDisabled()
         */
        public void projectionDisabled()
        {
            handleProjectionDisabled();
        }
    }

    @Override
    public void install(ProjectionViewer viewer)
    {

        internalUninstall();

        PHPStructuredTextViewer viewer1 = (PHPStructuredTextViewer) viewer;
        ITextEditor editor = viewer1.getTextEditor();

        this.viewer = viewer1;

        if (editor instanceof PHPStructuredEditor) {

            PHPStructuredEditor phpEditor = (PHPStructuredEditor) editor;

            if (phpEditor.getSourceViwerConfiguration().getClass() == PHPStructuredTextViewerConfiguration.class) {
                phpFoldingProvider = new StructuredTextFoldingProviderPHP();
                phpFoldingProvider.install(viewer);
            } else {

                fProjectionListener = new ProjectionListener(viewer);
                fEditor = (PHPStructuredEditor) editor;
            }
        }
    }

    private void internalUninstall()
    {
        if (isInstalled()) {
            handleProjectionDisabled();
            fProjectionListener.dispose();
            fProjectionListener = null;
            fEditor = null;
        }
    }

    @Override
    public void uninstall()
    {

        if (phpFoldingProvider != null) {
            phpFoldingProvider.uninstall();
            return;
        }

        internalUninstall();

    }

    @Override
    public void initialize()
    {

        if (phpFoldingProvider != null) {
            phpFoldingProvider.initialize();
            return;
        }

        if (viewer != null) {
            fDocument = viewer.getDocument();

            // set projection viewer on new document's adapter factory
            if (viewer.getProjectionAnnotationModel() != null) {
                final ProjectionModelNodeAdapterFactoryHTML factory2 = getAdapterFactoryHTML(true);
                if (factory2 != null) {
                    factory2.addProjectionViewer(viewer);
                }

                addAllAdapters();
            }
        }

        fUpdatingCount++;
        try {
            update(createInitialContext());
        } finally {
            fUpdatingCount--;
        }

    }

    @SuppressWarnings("rawtypes")
    private void update(FoldingStructureComputationContext ctx)
    {

        Map<TwigProjectionAnnotation, Position> additions = new HashMap<TwigProjectionAnnotation, Position>();
        List<TwigProjectionAnnotation> deletions = new ArrayList<TwigProjectionAnnotation>();
        List<TwigProjectionAnnotation> updates = new ArrayList<TwigProjectionAnnotation>();

        computeFoldingStructure(ctx);
        Map<Object, Position> newStructure = ctx.fMap;
        Map<IModelElement, Object> oldStructure = computeCurrentStructure(ctx);

        Iterator<Object> e = newStructure.keySet().iterator();

        while (e.hasNext()) {
            TwigProjectionAnnotation newAnnotation = (TwigProjectionAnnotation) e
                    .next();
            Position newPosition = newStructure.get(newAnnotation);

            IModelElement element = newAnnotation.getElement();
            /*
             * See https://bugs.eclipse.org/bugs/show_bug.cgi?id=130472 and
             * https://bugs.eclipse.org/bugs/show_bug.cgi?id=127445 In the
             * presence of syntax errors, anonymous types may have a source
             * range offset of 0. When such a situation is encountered, we
             * ignore the proposed folding range: if no corresponding folding
             * range exists, it is silently ignored; if there *is* a matching
             * folding range, we ignore the position update and keep the old
             * range, in order to keep the folding structure stable.
             */
            boolean isMalformedAnonymousType = newPosition.getOffset() == 0
                    && element.getElementType() == IModelElement.TYPE;
            List annotations = (List) oldStructure.get(element);
            if (annotations == null) {
                if (!isMalformedAnonymousType)
                    additions.put(newAnnotation, newPosition);
            } else {
                Iterator x = annotations.iterator();
                boolean matched = false;
                while (x.hasNext()) {
                    Tuple tuple = (Tuple) x.next();
                    TwigProjectionAnnotation existingAnnotation = tuple.annotation;
                    Position existingPosition = tuple.position;
                    if (newAnnotation.isComment() == existingAnnotation
                            .isComment()) {
                        boolean updateCollapsedState = ctx.allowCollapsing()
                                && existingAnnotation.isCollapsed() != newAnnotation
                                        .isCollapsed();
                        if (!isMalformedAnonymousType
                                && existingPosition != null
                                && (!newPosition.equals(existingPosition) || updateCollapsedState)) {
                            existingPosition.setOffset(newPosition.getOffset());
                            existingPosition.setLength(newPosition.getLength());
                            if (updateCollapsedState)
                                if (newAnnotation.isCollapsed())
                                    existingAnnotation.markCollapsed();
                                else
                                    existingAnnotation.markExpanded();
                            updates.add(existingAnnotation);
                        }
                        matched = true;
                        x.remove();
                        break;
                    }
                }
                if (!matched)
                    additions.put(newAnnotation, newPosition);

                if (annotations.isEmpty())
                    oldStructure.remove(element);
            }
        }

        e = oldStructure.values().iterator();
        while (e.hasNext()) {
            List list = (List) e.next();
            int size = list.size();
            for (int i = 0; i < size; i++)
                deletions.add(((Tuple) list.get(i)).annotation);
        }

        match(deletions, additions, updates, ctx);

        Annotation[] deletedArray = deletions.toArray(new Annotation[deletions
                .size()]);
        Annotation[] changedArray = updates.toArray(new Annotation[updates
                .size()]);
        ctx.getModel().modifyAnnotations(deletedArray, additions, changedArray);

    }

    private void computeFoldingStructure(FoldingStructureComputationContext ctx)
    {
        IParent parent = (IParent) fInput;
        try {
            if (!(fInput instanceof ISourceReference))
                return;
            computeFoldingStructure(parent.getChildren(), ctx);
        } catch (ModelException x) {
        }
    }

    private void computeFoldingStructure(IModelElement[] elements,
            FoldingStructureComputationContext ctx) throws ModelException
    {
        for (int i = 0; i < elements.length; i++) {
            IModelElement element = elements[i];

            computeFoldingStructure(element, ctx);

            if (element instanceof IParent) {
                IParent parent = (IParent) element;
                computeFoldingStructure(parent.getChildren(), ctx);
            }
        }
    }

    protected void computeFoldingStructure(IModelElement element,
            FoldingStructureComputationContext ctx)
    {

        boolean collapse = false;
        boolean collapseCode = true;

        switch (element.getElementType()) {

        // TODO : ask DLTK to have include container
            case IModelElement.IMPORT_CONTAINER :
                collapse = ctx.collapseImportContainer();
                break;
            case IModelElement.TYPE :
                collapse = ctx.collapseTypes();
                break;
            case IModelElement.METHOD :
                collapse = ctx.collapseMembers();
                break;
            case IModelElement.FIELD :
                // class fields should be folded as well
                IModelElement parent = element.getParent();
                if (parent != null
                        && parent.getElementType() != IModelElement.TYPE) {
                    return;
                }
                collapse = ctx.collapseMembers();
                break;
            default :
                return;
        }

        IRegion[] regions = computeProjectionRanges((ISourceReference) element,
                ctx);
        if (regions.length > 0) {
            // comments
            // use the set to filter the duplicate comment IRegion,or sometimes
            // you will see the header comment is collapsed but with a expanded
            // iamge,this is because there are two ProjectionAnnotations for one
            // comment,and the second ProjectionAnnotation's image overide the
            // header one
            Set<IRegion> regionSet = new HashSet<IRegion>();
            for (int i = 0; i < regions.length - 1; i++) {
                IRegion normalized = alignRegion(regions[i], ctx);
                if (normalized != null && regionSet.add(normalized)) {
                    Position position = createCommentPosition(normalized);
                    if (position != null) {
                        boolean commentCollapse;
                        if (i == 0
                                && (regions.length > 2 || ctx
                                        .hasHeaderComment())
                                && element == ctx.getFirstElement()) {
                            commentCollapse = ctx.collapseHeaderComments();
                        } else {
                            commentCollapse = ctx.collapseJavadoc();
                        }
                        ctx.addProjectionRange(new TwigProjectionAnnotation(
                                commentCollapse, element, true), position);
                    }
                }
            }
            // code
            if (collapseCode) {
                IRegion normalized = alignRegion(regions[regions.length - 1],
                        ctx);
                if (normalized != null) {
                    Position position = element instanceof IMember
                            ? createMemberPosition(normalized,
                                    (IMember) element)
                            : createCommentPosition(normalized);
                    if (position != null)
                        ctx.addProjectionRange(new TwigProjectionAnnotation(
                                collapse, element, false), position);
                }
            }
        }
    }

    protected final Position createCommentPosition(IRegion aligned)
    {
        return new CommentPosition(aligned.getOffset(), aligned.getLength());
    }

    protected final Position createMemberPosition(IRegion aligned,
            IMember member)
    {
        return new TwigElementPosition(aligned.getOffset(),
                aligned.getLength(), member);
    }

    protected final IRegion[] computeProjectionRanges(
            ISourceReference reference, FoldingStructureComputationContext ctx)
    {
        try {
            ISourceRange range = reference.getSourceRange();
            if (!SourceRange.isAvailable(range))
                return new IRegion[0];
            List<IRegion> regions = new ArrayList<IRegion>();
            if (!ctx.isHeaderChecked() && reference instanceof IModelElement) {
                ctx.setFirstElement((IModelElement) reference);
                ctx.setHeaderChecked();

                IRegion headerComment = computeHeaderComment(ctx);
                if (headerComment != null) {
                    regions.add(headerComment);
                    ctx.setHasHeaderComment();
                }
            }

            final int shift = range.getOffset();
            ICommentScanner scanner = ctx.getScanner();
            scanner.resetTo(shift);

            int start = shift;

            start = scanner.computePreviousComment();

            if (start != shift) {
                start = scanner.getCurrentCommentStartPosition();
                int end = scanner.getCurrentCommentEndPosition();
                regions.add(new Region(start, end - start));
            }

            // shift -> start
            regions.add(new Region(shift, range.getLength()));

            IRegion[] result = new IRegion[regions.size()];
            regions.toArray(result);
            return result;
        } catch (ModelException e) {
        }

        return new IRegion[0];
    }

    protected final IRegion alignRegion(IRegion region,
            FoldingStructureComputationContext ctx)
    {
        if (region == null)
            return null;

        IDocument document = ctx.getDocument();

        try {

            int start = document.getLineOfOffset(region.getOffset());
            int end = document.getLineOfOffset(Math.min(region.getOffset()
                    + region.getLength() - 1, document.getLength()));
            if (start >= end)
                return null;

            int offset = document.getLineOffset(start);
            int endOffset;
            if (document.getNumberOfLines() > end + 1)
                endOffset = document.getLineOffset(end + 1);
            else
                endOffset = document.getLineOffset(end)
                        + document.getLineLength(end);

            return new Region(offset, endOffset - offset);

        } catch (BadLocationException x) {
            // concurrent modification
            return null;
        }
    }

    private void match(List<TwigProjectionAnnotation> deletions,
            Map<TwigProjectionAnnotation, Position> additions,
            List<TwigProjectionAnnotation> changes,
            FoldingStructureComputationContext ctx)
    {
        if (deletions.isEmpty() || (additions.isEmpty() && changes.isEmpty()))
            return;

        List<TwigProjectionAnnotation> newDeletions = new ArrayList<TwigProjectionAnnotation>();
        List<TwigProjectionAnnotation> newChanges = new ArrayList<TwigProjectionAnnotation>();

        Iterator<TwigProjectionAnnotation> deletionIterator = deletions
                .iterator();
        while (deletionIterator.hasNext()) {
            TwigProjectionAnnotation deleted = deletionIterator.next();
            Position deletedPosition = ctx.getModel().getPosition(deleted);
            if (deletedPosition == null)
                continue;

            Tuple deletedTuple = new Tuple(deleted, deletedPosition);

            Tuple match = findMatch(deletedTuple, changes, null, ctx);
            boolean addToDeletions = true;
            if (match == null) {
                match = findMatch(deletedTuple, additions.keySet(), additions,
                        ctx);
                addToDeletions = false;
            }

            if (match != null) {
                IModelElement element = match.annotation.getElement();
                deleted.setElement(element);
                deletedPosition.setLength(match.position.getLength());
                if (deletedPosition instanceof TwigElementPosition
                        && element instanceof IMember) {
                    TwigElementPosition jep = (TwigElementPosition) deletedPosition;
                    jep.setMember((IMember) element);
                }

                deletionIterator.remove();
                newChanges.add(deleted);

                if (addToDeletions)
                    newDeletions.add(match.annotation);
            }
        }

        deletions.addAll(newDeletions);
        changes.addAll(newChanges);
    }

    private Tuple findMatch(Tuple tuple,
            Collection<TwigProjectionAnnotation> annotations,
            Map<TwigProjectionAnnotation, Position> positionMap,
            FoldingStructureComputationContext ctx)
    {
        Iterator<TwigProjectionAnnotation> it = annotations.iterator();
        while (it.hasNext()) {
            TwigProjectionAnnotation annotation = it.next();
            if (tuple.annotation.isComment() == annotation.isComment()) {
                Position position = positionMap == null ? ctx.getModel()
                        .getPosition(annotation) : positionMap.get(annotation);
                if (position == null)
                    continue;

                if (tuple.position.getOffset() == position.getOffset()) {
                    it.remove();
                    return new Tuple(annotation, position);
                }
            }
        }

        return null;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Map<IModelElement, Object> computeCurrentStructure(
            FoldingStructureComputationContext ctx)
    {
        Map<IModelElement, Object> map = new HashMap<IModelElement, Object>();
        ProjectionAnnotationModel model = ctx.getModel();
        Iterator e = model.getAnnotationIterator();

        while (e.hasNext()) {

            Object annotation = e.next();
            if (annotation instanceof TwigProjectionAnnotation) {
                TwigProjectionAnnotation java = (TwigProjectionAnnotation) annotation;
                Position position = model.getPosition(java);
                Assert.isNotNull(position);
                List<Tuple> list = (List<Tuple>) map.get(java.getElement());
                if (list == null) {
                    list = new ArrayList<Tuple>(2);
                    map.put(java.getElement(), list);
                }
                list.add(new Tuple(java, position));
            }
        }

        Comparator comparator = new Comparator()
        {
            public int compare(Object o1, Object o2)
            {
                return ((Tuple) o1).position.getOffset()
                        - ((Tuple) o2).position.getOffset();
            }
        };
        for (Iterator<Object> it = map.values().iterator(); it.hasNext();) {
            List list = (List) it.next();
            Collections.sort(list, comparator);
        }
        return map;
    }

    private IRegion computeHeaderComment(FoldingStructureComputationContext ctx)
            throws ModelException
    {
        if (!(ctx.getDocument() instanceof IStructuredDocument)) {
            return null;
        }
        final IStructuredDocument document = (IStructuredDocument) ctx
                .getDocument();
        IStructuredDocumentRegion sdRegion = document
                .getFirstStructuredDocumentRegion();
        int i = 0;
        while (sdRegion != null
                && sdRegion.getType() != TwigRegionTypes.TWIG_CONTENT
                && i++ < 40) {
            sdRegion = sdRegion.getNext();
        }

        if (sdRegion == null
                || sdRegion.getType() != TwigRegionTypes.TWIG_CONTENT
                || sdRegion.getRegions().size() < 2) {
            return null;
        }

        final ITwigScriptRegion textRegion = (ITwigScriptRegion) sdRegion
                .getRegions().get(1);
        try {
            ITextRegion phpToken = textRegion.getTwigToken(0);
            i = 0;
            while (phpToken != null
                    && phpToken.getType() != /*
                                              * PHPRegionTypes.PHPDOC_COMMENT_START
                                              */TwigRegionTypes.TWIG_COMMENT_TEXT
                    && i++ < 3) {
                phpToken = textRegion.getTwigToken(phpToken.getEnd() + 1);
            }
            if (phpToken == null
                    || phpToken.getType() != /*
                                              * PHPRegionTypes.PHPDOC_COMMENT_START
                                              */TwigRegionTypes.TWIG_COMMENT_TEXT) {
                return null;
            }
            int start = phpToken.getStart();
            ITextRegion lastToken = null;
            while (lastToken != phpToken
                    && phpToken != null
                    && phpToken.getType() != /*
                                              * PHPRegionTypes.PHPDOC_COMMENT_END
                                              */TwigRegionTypes.TWIG_COMMENT_TEXT) {
                phpToken = textRegion.getTwigToken(phpToken.getEnd() + 1);
            }

            if (phpToken != null
                    && phpToken.getType() == /*
                                              * PHPRegionTypes.PHPDOC_COMMENT_END
                                              */TwigRegionTypes.TWIG_COMMENT_TEXT) {
                int end = phpToken.getEnd();
                return new Region(sdRegion.getStartOffset()
                        + textRegion.getStart() + start, end - start);
            }
            return null;

        } catch (BadLocationException e) {
            return null;
        }
    }

    private ProjectionAnnotationModel getModel()
    {

        return this.viewer.getProjectionAnnotationModel();
    }

    protected final boolean isInstalled()
    {
        return fEditor != null;
    }

    private FoldingStructureComputationContext createInitialContext()
    {
        initializePreferences();
        fInput = getInputElement();
        if (fInput == null)
            return null;

        return createContext(true);
    }

    private FoldingStructureComputationContext createContext(
            boolean allowCollapse)
    {
        if (!isInstalled())
            return null;
        ProjectionAnnotationModel model = getModel();
        if (model == null)
            return null;
        IDocument doc = getDocument();
        if (doc == null)
            return null;

        return new FoldingStructureComputationContext(doc, model, allowCollapse);
    }

    private IDocument getDocument()
    {
        PHPStructuredEditor editor = fEditor;
        if (editor == null)
            return null;

        IDocumentProvider provider = editor.getDocumentProvider();
        if (provider == null)
            return null;

        return provider.getDocument(editor.getEditorInput());
    }

    private void initializePreferences()
    {
        IPreferenceStore store = PHPUiPlugin.getDefault().getPreferenceStore();
        // fCollapseImportContainer=
        // store.getBoolean(PreferenceConstants.EDITOR_FOLDING_IMPORTS);
        fCollapseTypes = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_CLASSES);
        fCollapsePhpdoc = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_PHPDOC);
        fCollapseMembers = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_FUNCTIONS);
        fCollapseHeaderComments = store
                .getBoolean(PreferenceConstants.EDITOR_FOLDING_HEADER_COMMENTS);
    }

    private IModelElement getInputElement()
    {
        if (fEditor == null)
            return null;
        return EditorUtility.getEditorInputModelElement(fEditor, false);
    }

    @Override
    public void projectionEnabled()
    {

        if (phpFoldingProvider != null) {
            phpFoldingProvider.projectionEnabled();
            return;
        }

        handleProjectionEnabled();

    }

    @Override
    public void projectionDisabled()
    {

        if (phpFoldingProvider != null) {
            phpFoldingProvider.projectionDisabled();
            return;
        }

        handleProjectionDisabled();
    }

    protected void handleProjectionEnabled()
    {

        handleProjectionDisabled();

        if (isInstalled()) {
            initialize();
            fElementListener = new ElementChangedListener();
            DLTKCore.addElementChangedListener(fElementListener);
        }
    }

    /**
     * Called whenever projection is disabled, for example when the provider is
     * {@link #uninstall() uninstalled}, when the viewer issues a
     * {@link IProjectionListener#projectionDisabled() projectionDisabled}
     * message and before {@link #handleProjectionEnabled() enabling} the
     * provider. Implementations must be prepared to handle multiple calls to
     * this method even if the provider is already disabled.
     * <p>
     * Subclasses may extend.
     * </p>
     */
    protected void handleProjectionDisabled()
    {

        if (fElementListener != null) {
            DLTKCore.removeElementChangedListener(fElementListener);
            fElementListener = null;
        }

        final ProjectionModelNodeAdapterFactoryHTML factory2 = getAdapterFactoryHTML(false);
        if (factory2 != null) {
            factory2.removeProjectionViewer(viewer);
        }

        // clear out all annotations
        if (viewer.getProjectionAnnotationModel() != null) {
            viewer.getProjectionAnnotationModel().removeAllAnnotations();
        }

        removeAllAdapters();

        fDocument = null;
    }

    private ProjectionModelNodeAdapterFactoryHTML getAdapterFactoryHTML(
            boolean createIfNeeded)
    {
        ProjectionModelNodeAdapterFactoryHTML factory = null;
        if (fDocument != null) {
            IStructuredModel sModel = null;
            try {
                sModel = StructuredModelManager.getModelManager()
                        .getExistingModelForRead(fDocument);
                if (sModel != null && (sModel instanceof IDOMModel)) {
                    final FactoryRegistry factoryRegistry = sModel
                            .getFactoryRegistry();

                    // getting the projectionmodelnodeadapter for the first
                    // time
                    // so do some initializing
                    if (!factoryRegistry
                            .contains(ProjectionModelNodeAdapterHTML.class)
                            && createIfNeeded) {
                        final ProjectionModelNodeAdapterFactoryHTML newFactory = new ProjectionModelNodeAdapterFactoryHTML();

                        // add factory to factory registry
                        factoryRegistry.addFactory(newFactory);

                        // add factory to propogating adapter
                        final IDOMModel domModel = (IDOMModel) sModel;
                        final IDOMDocument document = domModel.getDocument();
                        final PropagatingAdapter propagatingAdapter = (PropagatingAdapter) ((INodeNotifier) document)
                                .getAdapterFor(PropagatingAdapter.class);
                        if (propagatingAdapter != null) {
                            propagatingAdapter
                                    .addAdaptOnCreateFactory(newFactory);
                        }
                    }

                    // try and get the factory
                    factory = (ProjectionModelNodeAdapterFactoryHTML) factoryRegistry
                            .getFactoryFor(ProjectionModelNodeAdapterHTML.class);
                }
            } finally {
                if (sModel != null) {
                    sModel.releaseFromRead();
                }
            }
        }

        return factory;
    }

    private void addAllAdapters()
    {
        if (fDocument != null) {
            IStructuredModel sModel = null;
            try {
                sModel = StructuredModelManager.getModelManager()
                        .getExistingModelForRead(fDocument);
                if (sModel != null) {
                    IndexedRegion startNode = sModel.getIndexedRegion(0);
                    if (startNode == null) {
                        assert sModel instanceof IDOMModel;
                        startNode = ((IDOMModel) sModel).getDocument();
                    }

                    if (startNode instanceof Node) {
                        int siblingLevel = 0;
                        Node nextSibling = (Node) startNode;

                        while (nextSibling != null
                                && siblingLevel < MAX_SIBLINGS) {
                            final Node currentNode = nextSibling;
                            nextSibling = currentNode.getNextSibling();

                            addAdapterToNodeAndChildrenHTML(currentNode, 0);
                            ++siblingLevel;
                        }
                    }
                }
            } finally {
                if (sModel != null) {
                    sModel.releaseFromRead();
                }
            }
        }
    }

    private void addAdapterToNodeAndChildrenHTML(Node node, int childLevel)
    {
        // stop adding initial adapters MAX_CHILDREN levels deep for
        // performance sake
        if (node instanceof INodeNotifier && childLevel < MAX_CHILDREN) {
            final INodeNotifier notifier = (INodeNotifier) node;

            // try and get the adapter for the current node and update the
            // adapter with projection information
            final ProjectionModelNodeAdapterHTML adapter2 = (ProjectionModelNodeAdapterHTML) notifier
                    .getExistingAdapter(ProjectionModelNodeAdapterHTML.class);
            if (adapter2 != null) {
                adapter2.updateAdapter(node);
            } else {
                // just call getadapter so the adapter is created and
                // automatically initialized
                notifier.getAdapterFor(ProjectionModelNodeAdapterHTML.class);
            }
            int siblingLevel = 0;
            Node nextChild = node.getFirstChild();
            while (nextChild != null && siblingLevel < MAX_SIBLINGS) {
                final Node childNode = nextChild;
                nextChild = childNode.getNextSibling();

                addAdapterToNodeAndChildrenHTML(childNode, childLevel + 1);
                ++siblingLevel;
            }
        }
    }

    private void removeAllAdapters()
    {
        if (fDocument != null) {
            IStructuredModel sModel = null;
            try {
                sModel = StructuredModelManager.getModelManager()
                        .getExistingModelForRead(fDocument);
                if (sModel != null) {
                    final int startOffset = 0;
                    final IndexedRegion startNode = sModel
                            .getIndexedRegion(startOffset);
                    if (startNode instanceof Node) {
                        Node nextSibling = (Node) startNode;
                        while (nextSibling != null) {
                            final Node currentNode = nextSibling;
                            nextSibling = currentNode.getNextSibling();

                            removeAdapterFromNodeAndChildren(currentNode, 0);
                        }
                    }
                }
            } finally {
                if (sModel != null) {
                    sModel.releaseFromRead();
                }
            }
        }

    }

    private void removeAdapterFromNodeAndChildren(Node node, int level)
    {
        if (node instanceof INodeNotifier) {
            final INodeNotifier notifier = (INodeNotifier) node;

            // try and get the adapter for the current node and remove it
            final INodeAdapter adapter2 = notifier
                    .getExistingAdapter(ProjectionModelNodeAdapterHTML.class);
            if (adapter2 != null) {
                notifier.removeAdapter(adapter2);
            }

            Node nextChild = node.getFirstChild();
            while (nextChild != null) {
                final Node childNode = nextChild;
                nextChild = childNode.getNextSibling();

                removeAdapterFromNodeAndChildren(childNode, level + 1);
            }
        }
    }

    private class ElementChangedListener implements IElementChangedListener
    {

        /*
         * @see
         * org.eclipse.jdt.core.IElementChangedListener#elementChanged(org.eclipse
         * .jdt.core.ElementChangedEvent)
         */
        public void elementChanged(ElementChangedEvent e)
        {
            IModelElementDelta delta = findElement(fInput, e.getDelta());
            if (delta != null
                    && (delta.getFlags() & (IModelElementDelta.F_CONTENT | IModelElementDelta.F_CHILDREN)) != 0) {

                if (shouldIgnoreDelta(e.getDelta().getElement(), delta))
                    return;

                fUpdatingCount++;
                try {
                    update(createContext(false));
                } finally {
                    fUpdatingCount--;
                }
            }
        }

        /**
         * Ignore the delta if there are errors on the caret line.
         * <p>
         * We don't ignore the delta if an import is added and the caret isn't
         * inside the import container.
         * </p>
         * 
         * @param ast
         *            the compilation unit AST
         * @param delta
         *            the Java element delta for the given AST element
         * @return <code>true</code> if the delta should be ignored
         * @since 3.3
         */
        private boolean shouldIgnoreDelta(IModelElement ast,
                IModelElementDelta delta)
        {
            if (ast == null)
                return false; // can't compute

            if (!(ast.getResource() instanceof IFile)) {
                return false;
            }
            final IFile resource = (IFile) ast.getResource();

            IDocument document = getDocument();
            if (document == null)
                return false; // can't compute

            PHPStructuredEditor editor = fEditor;
            if (editor == null || editor.getCachedSelectedRange() == null)
                return false; // can't compute

            int caretLine = 0;
            try {
                caretLine = document.getLineOfOffset(editor
                        .getCachedSelectedRange().x) + 1;
            } catch (BadLocationException x) {
                return false; // can't compute
            }

            if (caretLine > 0) {
                try {
                    IMarker[] problems = resource.findMarkers(
                            DefaultProblem.MARKER_TYPE_PROBLEM, false,
                            IResource.DEPTH_INFINITE);
                    for (int i = 0; i < problems.length; i++) {
                        final IMarker marker = problems[i];
                        final boolean isInCaret = isCaretLine(caretLine,
                                IMarker.LINE_NUMBER, marker);
                        final boolean isSyntaxError = isCaretLine(
                                IMarker.SEVERITY_ERROR, IMarker.SEVERITY,
                                marker);
                        if (isSyntaxError && isInCaret) {
                            return true;
                        }
                    }

                } catch (CoreException e) {
                    return false;
                }
            }
            return false;
        }

        private final boolean isCaretLine(int expected, String attribute,
                final IMarker marker) throws CoreException
        {
            final Object res = marker.getAttribute(attribute); // IMarker.LINE_NUMBER
            return res != null && res instanceof Integer
                    && (Integer) res == expected;
        }

        private IModelElementDelta findElement(IModelElement target,
                IModelElementDelta delta)
        {

            if (delta == null || target == null)
                return null;

            IModelElement element = delta.getElement();

            if (element.getElementType() > IModelElement.BINARY_MODULE)
                return null;

            if (target.equals(element))
                return delta;

            IModelElementDelta[] children = delta.getAffectedChildren();

            for (int i = 0; i < children.length; i++) {
                IModelElementDelta d = findElement(target, children[i]);
                if (d != null)
                    return d;
            }

            return null;
        }
    }
}
