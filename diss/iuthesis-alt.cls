%%
%% This is file `iuthesis.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% iuthesis.dtx  (with options: `class')
%%   IUThesis Document Class (N. Danner)

%% iuthesis:  Document class for IU Thesis.
%% N. Danner (ndanner@indiana.edu)
%% Copyright 1999

%% RCS Info:
%%      iuthesis.dtx,v 1.2.0.23 1999/06/04 14:30:00 ndanger Exp



\def\fileversion{1.2.0.23}
\def\filedate{1999/06/04}

\NeedsTeXFormat{LaTeX2e}[1996/12/01] \ProvidesClass{iuthesis}
\message{<< Class 'iuthesis', v\fileversion, \filedate (N. Danner) >>}

\newif\ifIUT@debugging \IUT@debuggingtrue \IUT@debuggingfalse
\def\IUT@dbgmsg#1{\ifIUT@debugging\message{#1}\fi}
\def\IUT@optionmsg#1{\message{  iuthesis option: #1}}

\def\IUTdraftmsg#1{}
\let\draftmsg\IUTdraftmsg
\def\IUT@showdraftfoot{}
\DeclareOption{draft}{
  \IUT@optionmsg{draft}
  \def\IUTdraftmsg#1{\gdef\IUT@showdraftfoot{#1}}
  \let\draftmsg\IUTdraftmsg
  \def\IUT@showdraftfoot{DRAFT: \today}
  \PassOptionsToClass{draft}{amsbook}
}

\DeclareOption{final}{
  \IUT@optionmsg{final}
  \def\IUTdraftmsg#1{}
  \let\draftmsg\IUTdraftmsg
  \def\IUT@showdraftfoot{}
  \PassOptionsToClass{final}{amsbook}
}

\newcount\IUT@abstype
\newcount\IUT@nmlabstract\IUT@nmlabstract=0
\newcount\IUT@umiabstract\IUT@umiabstract=1
\newcount\IUT@ugsabstract\IUT@ugsabstract=2
\IUT@abstype=\IUT@nmlabstract
\DeclareOption{umiabstract}{
  \IUT@optionmsg{umiabstract}
  \IUT@abstype=\IUT@umiabstract
  \AtEndOfClass{\raggedbottom}
}
\newif\ifIUT@ugsabs
\IUT@ugsabsfalse
\DeclareOption{ugsabstract}{
  \IUT@optionmsg{ugsabstract}
  \IUT@abstype=\IUT@ugsabstract
  \AtEndOfClass{\raggedbottom}
}

\newif\ifIUT@abstractsigs
\DeclareOption{abstractsigs}{
  \IUT@optionmsg{abstractsigs}
  \IUT@abstractsigstrue
}
\DeclareOption{noabstractsigs}{
  \IUT@optionmsg{noabstractsigs}
  \IUT@abstractsigsfalse
}

\newif\ifIUT@noabstract
\IUT@noabstractfalse
\DeclareOption{noabstract}{
  \IUT@optionmsg{noabstract}
  \IUT@noabstracttrue
}

\DeclareOption{ms}{
  \AtEndOfClass{\degree{Master of Science}}
}
\DeclareOption{ma}{
  \AtEndOfClass{\degree{Master of Arts}}
}

\newif\ifIUT@three
\DeclareOption{three}{\IUT@threetrue}

\DeclareOption{single}{
  \AtEndOfClass{\def\IUT@blstretch{1}}
}

\DeclareOption{debug}{
  \IUT@debuggingtrue
}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{amsbook}}
\ExecuteOptions{abstractsigs,draft}
\ProcessOptions

\PassOptionsToClass{oneside}{amsbook}
\LoadClass{amsbook}[1995/01/27]

\topmargin 0pt
\oddsidemargin=.5in
\evensidemargin=\oddsidemargin
\advance\headheight 2pt
\textwidth 6in
\textheight 8.9in
\advance\textheight by -\headheight
\advance\textheight by -\headsep
\advance\textheight by -\footskip
\marginparwidth 0.5in

\def\ps@chapter{
  \gdef\ps@headings{
    \def\@oddhead{
      \normalfont\scriptsize\hfil\rightmark{}{}\hfil \llap{}
    }
    \def\@oddfoot{
      \normalfont\scriptsize\rlap{\ttfamily\IUT@showdraftfoot\hfill}%
      \hfil\thepage\hfil
    }
    \global\let\@evenfoot\@oddfoot
    \global\let\@mkboth\markboth
    \global\let\partmark\@gobble
    \gdef\chaptermark{%
      \@secmark\markboth\chapterrunhead{}}%
    \global\let\sectionmark\@gobble% this controls the running headings
  }
}

\def\ps@section{
  \gdef\ps@headings{
    \def\@oddhead{
      \normalfont\scriptsize\hfil\rightmark{}{}\hfil \llap{}
    }
    \def\@oddfoot{
      \normalfont\scriptsize\rlap{\ttfamily\IUT@showdraftfoot\hfill}%
      \hfil\thepage\hfil
    }
    \global\let\@evenfoot\@oddfoot
    \global\let\@mkboth\markboth
    \global\let\partmark\@gobble
    \gdef\chaptermark{%
      \@secmark\markboth\chapterrunhead{}}%
    \gdef\sectionmark{%
      \@secmark\markboth\sectionrunhead{}}%
  }
}

\def\ps@chapsec{
  \gdef\ps@headings{
    \def\@oddhead{
      \normalfont\scriptsize\rlap{\leftmark{}{}}\hfill\llap{\rightmark{}{}}
    }
    \def\@oddfoot{
      \normalfont\scriptsize\rlap{\ttfamily\IUT@showdraftfoot\hfill}%
      \hfil\thepage\hfil
    }
    \global\let\@evenfoot\@oddfoot
    \global\let\@mkboth\markboth
    \global\let\partmark\@gobble
    \gdef\chaptermark{%
      \@secmark\markboth\chapterrunhead\chaptername}%
    \gdef\sectionmark{
      \@secmark\markright\IUT@sectionrunhead{}}
  }
}

\def\IUT@sectionrunhead#1#2#3{%
  \def\@tempa{#3}%
    \ifx\@empty\@tempa\else%
    \@ifnotempty{#2}{\uppercase{#1 #2}\@ifnotempty{#3}{. }}%
    \ifx\@empty\@tempa\else\uppercasenonmath\@tempa\@tempa\fi%
  \fi%
}

\def\ps@plain{\ps@empty
  \def\@oddfoot{
    \normalfont\scriptsize
    \rlap{\ttfamily\IUT@showdraftfoot}\hfil\thepage\hfil
  }
  \let\@evenfoot\@oddfoot
}

\renewcommand{\author}[1]{\gdef\@author{#1}}\let\@author\relax
\renewcommand{\title}[1]{\gdef\@title{#1}\uppercasenonmath\@title}\let\@title\relax

\def\acceptdate#1{\gdef\@acceptdate{#1}}\let\@acceptdate\relax
\def\submitdate#1{\gdef\@submitdate{#1}}\let\@submitdate\relax
\def\department#1{\gdef\@department{#1}}\let\@department\relax
\def\departmentname#1{\gdef\@departmentname{#1}}\departmentname{Department}
\def\degree#1{\gdef\@degree{#1}}\degree{Doctor of Philosophy}

\def\advisor#1{\gdef\@principaladvisor{#1}}\let\@principaladvisor\relax
\def\secondreader#1{\gdef\@secondreader{#1}}\let\@secondreader\relax
\def\thirdreader#1{\gdef\@thirdreader{#1}}\let\@thirdreader\relax
\def\fourthreader#1{\gdef\@fourthreader{#1}}\let\@fourthreader\relax
\def\fifthreader#1{\gdef\@fifthreader{#1}}\let\@fifthreader\relax

\def\copyrightyear#1{\gdef\@copyrightyear{#1}}
\let\@copyrightyear\relax



\newbox\dedicationbox
\newenvironment{dedication}{%
  \global\let\dedication\relax%
  \bgroup
  \renewcommand{\baselinestretch}{1}
  \normalsize
  \global\setbox\dedicationbox\vbox\bgroup%
  \begin{center}
}{
  \end{center}
  \egroup\egroup
}


\newbox\acknowledgebox
\newenvironment{acknowledgements}{%
  \global\let\acknowledgements\relax%
  \bgroup
  \renewcommand{\baselinestretch}{\IUT@blstretch}
  \normalsize
  \global\setbox\acknowledgebox\vbox\bgroup
  \trivlist%
    \item[]\ignorespaces
}{
  \endtrivlist
  \egroup\egroup
}

\renewenvironment{abstract}{%
  \global\let\abstract\relax%
  \bgroup%
  \renewcommand{\baselinestretch}{\IUT@blstretch}%
  \normalsize%
  \global\setbox\abstractbox\vbox\bgroup
  \trivlist\item[]\ignorespaces
}%
{
  \endtrivlist
  \egroup\egroup
  \global\let\endabstract\relax
}

\def\IUT@defineabstract{
  \gdef\makeabstract{
    \typeout{Abstract}
    \bgroup
    \normalfont
    \ifnum\IUT@abstype>0%
      \IUT@dbgmsg{makeabstract: Setting abstract pagestyle empty}%
      \pagestyle{empty}\thispagestyle{empty}%
    \else%
      \IUT@dbgmsg{makeabstract: Setting abstract pagestlye plain}%
    \fi%
    \ifnum\IUT@abstype>0%
      \IUT@dbgmsg{makeabstract: Setting title}%
      \begin{center}%
      \check@val\@author \\[.5\baselineskip]
      \check@val\@title
      \end{center}%
      \vskip\baselineskip
      \IUT@dbgmsg{makeabstract: Done}%
    \else%
      \IUT@dbgmsg{makeabstract: Setting title}%
      %\@makeschapterhead{Abstract}
      \@makeschapterhead{\@title}
      \vspace{-20pt}
      \begin{flushright}\@author\end{flushright}
      \vspace{20pt}
    \fi%
    \IUT@dbgmsg{makeabstract: Setting abstract}%
    \unvbox\abstractbox
    \ifnum\IUT@abstype=\IUT@umiabstract%
      \IUT@dbgmsg{makeabstract (umi): advisor's name}%
      \vskip\baselineskip%
      \hbox to\hsize{\hfill\check@val\@principaladvisor, Ph.D.}%
    \fi
    \ifnum\IUT@abstype=\IUT@ugsabstract%
      \IUT@dbgmsg{makeabstract (ugs): signature lines}%
      \ifIUT@abstractsigs%
      \vbox{
\hbox to\textwidth{%
  \IUT@sig[2.5in]{\check@val\@principaladvisor, Ph.D.}\hfill%
  \IUT@sig[2.5in]{\check@val\@thirdreader, Ph.D.}%
}%
\hbox to\textwidth{%
  \IUT@sig[2.5in]{\check@val\@secondreader, Ph.D.}\hfill%
  \ifIUT@three\else\IUT@sig[2.5in]{\check@val\@fourthreader, Ph.D.}\fi%
}%
\ifx\@fifthreader\relax\else%
  \hbox to\textwidth{%
    \hfill\IUT@sig[2.5in]{\check@val\@fifthreader, Ph.D.}%
  }%
\fi%
      }
      \fi
    \fi
    \newpage
    \egroup
    \ifnum\IUT@abstype>0
      \def\end@fterabstract{\end{document}}
    \else
      \def\end@fterabstract{}
    \fi
    \end@fterabstract
  }
}

\ifIUT@noabstract
  \ifnum\IUT@abstype>0\IUT@defineabstract\else\def\makeabstract{}\fi
\else
  \IUT@defineabstract
\fi

\ifnum\IUT@abstype>1
  \IUT@dbgmsg{Setting maketitle noop}
  \def\maketitle{}
\else
  \def\maketitle{
    \bgroup
    \thispagestyle{empty}
    \hbox{}\vskip1in
    \bgroup
    \LARGE\bfseries
    \begin{center}
    \check@val\@title
    \end{center}
    \egroup
    \vfill
    \bgroup
    \Large
    \begin{center}
    \check@val\@author
    \end{center}
    \egroup
    \vfill
    \begin{center}
Submitted to the faculty of the University Graduate School \\
in partial fulfillment of the requirements \\
for the degree \\
\check@val\@degree \\
in the \check@val\@departmentname\ of \check@val\@department \\
Indiana University \\
\check@val\@submitdate
%    \check@val\@submitdate
    \end{center}
    \vskip.5in
    \egroup
    \newpage
  }
\fi

\ifnum\IUT@abstype>0
  \IUT@dbgmsg{Setting frontmatter commands noops}
  \def\signaturepage{}
  \def\copyrightpage{}
  \newcommand{\makededication}[1][]{}
  \def\makeack{}
\else
  \def\signaturepage{
    \typeout{Signature Page}
    \bgroup
    \noindent
    Accepted by the Graduate Faculty, Indiana University, in partial
    fulfillment of the requirements for the degree of \check@val\@degree.
    \egroup
    \vskip.5in
    \hbox to\textwidth{\hfill\IUT@sig{\check@val\@principaladvisor, Ph.D.}}%
    \hbox to\textwidth{\hfill\IUT@sig{\check@val\@secondreader, Ph.D.}}%
    \hbox to\textwidth{\hfill\IUT@sig{\check@val\@thirdreader, Ph.D.}}%
    \ifIUT@three\else%
      \hbox to\textwidth{\hfill\IUT@sig{\check@val\@fourthreader, Ph.D.}}%
    \fi%
    \ifx\@fifthreader\relax\else%
      \hbox to\textwidth{\hfill\IUT@sig{\check@val\@fifthreader, Ph.D.}}%
    \fi
    \vfill
    \noindent\check@val\@acceptdate
    \vfill
    \newpage
  }

  \def\copyrightpage{
    \typeout{Copyright Page}
    \hbox{}\vfill
    \begin{center}
    Copyright \check@val\@copyrightyear \\
    \check@val\@author \\
    All rights reserved
    \end{center}
    \vfill
    \newpage
  }

  \newcommand{\makededication}[1][1in]{
    \ifvoid\dedicationbox\else
    \typeout{Dedication}
    \hbox{}\vskip#1\unvbox\dedicationbox\vfill%
    \newpage
    \fi
  }


  \def\makeack{
    \ifvoid\acknowledgebox\else
    \typeout{Acknowledgements}
    \@makeschapterhead{Acknowledgements}
    \unvbox\acknowledgebox
    \newpage
    \fi
  }

\fi

\newcommand{\IUT@sig}[2][3in]{
   \vbox{%
    \hrule width 0pt height .6in depth 0pt%
    \hrule width #1 height .4pt depth 0pt%
    \vskip2mm%
    \hbox to #1{\hfill #2}%
   }
}

\def\tableofcontents{
  \bgroup
  \renewcommand{\baselinestretch}{\IUT@blstretch}
  \normalfont
  \@starttoc{toc}\contentsname
  \egroup
}
\def\listoffigures{
  \bgroup
  \renewcommand{\baselinestretch}{\IUT@blstretch}
  \normalfont
  \@starttoc{lof}\listfigurename
  \egroup
}
\def\listoftables{
  \bgroup
  \renewcommand{\baselinestretch}{\IUT@blstretch}
  \normalfont
  \@starttoc{lot}\listtablename
  \egroup
}

\def\frontmatter{\cleardoublepage\pagenumbering{roman}\pagestyle{plain}}

\def\mainmatter{
  \cleardoublepage
  \pagenumbering{arabic}
  \pagestyle{headings}
  \renewcommand{\baselinestretch}{\IUT@blstretch}
  \normalfont
}

\def\backmatter{
  \newpage
  \pagestyle{empty}
  \renewcommand{\baselinestretch}{1}
  \normalfont
}

\def\check@val#1{%
  \ifx#1\relax%
    \typeout{}%
    \typeout{!!!!!!!!}%
    \typeout{Warning: #1 not set!}%
    \typeout{!!!!!!!!}%
    \hbox{???}%
  \else%
    #1%
  \fi%
}

\partopsep=-\parskip

\def\section{\@startsection{section}{1}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\noindent}}
\def\subsection{\@startsection{subsection}{2}%
  \normalparindent{.5\linespacing\@plus.7\linespacing}{-.5em}%
  {\normalfont\bfseries\noindent}}
\def\subsubsection{\@startsection{subsubsection}{3}%
  \normalparindent\z@{-.5em}%
  {\normalfont\bfseries\noindent}}
\def\paragraph{\@startsection{paragraph}{4}%
  \normalparindent\z@{-\fontdimen2\font}%
  \normalfont}

\def\parheader#1{\noindent{\normalfont\bfseries {#1}.}}

\renewcommand{\seename}{see}

\def\IUT@blstretch{1.5}
\ifnum\@mainsize=10\def\IUT@blstretch{1.5}\fi
\ifnum\@mainsize=11\def\IUT@blstretch{1.5}\fi
\ifnum\@mainsize=12\def\IUT@blstretch{1.5}\fi
\IUT@dbgmsg{Baseline stretch: \IUT@blstretch}

\pagestyle{chapter}
\pagenumbering{arabic}
\normalsize



\endinput
%%
%% End of file `iuthesis.cls'.
