use miette::{NamedSource, SourceCode};
use std::borrow::Cow;
use std::fmt::{self, Debug, Display, Formatter};
use std::sync::OnceLock;

/// Convenience for `Debug`
#[allow(non_camel_case_types)]
#[derive(Debug)]
struct ROOT;

type InnerVec = boxcar::Vec<(Cow<'static, str>, boxcar::Vec<NamedSource<Vec<u8>>>)>;

/// The `FileRegistry` keeps track of all of the files. The only valid one is `FILE_REGISTRY`, and
/// no others can be constructed.
pub struct FileRegistry {
    files: OnceLock<InnerVec>,
}

impl FileRegistry {
    const fn new() -> Self {
        Self {
            files: OnceLock::new(),
        }
    }
    fn get(&self) -> &InnerVec {
        self.files
            .get_or_init(|| boxcar::vec![("".into(), boxcar::Vec::new())])
    }

    /// Add a package, get a `PackageId`. This allows the same file name in multiple packages.
    pub fn add_package<P: Into<Cow<'static, str>>>(&self, name: P) -> PackageId {
        PackageId(self.get().push((name.into(), boxcar::Vec::new())))
    }
    /// Add a file into a package. The contents don't have to be valid UTF-8.
    /// If you don't have a package, use `PackageId::ROOT`.
    pub fn add_file<N: AsRef<str>, C: Into<Vec<u8>>>(
        &self,
        pkg: PackageId,
        name: N,
        contents: C,
    ) -> FileId {
        let idx = self.get()[pkg.0]
            .1
            .push(NamedSource::new(name, contents.into()));
        FileId(pkg.0, idx)
    }
}

impl Debug for FileRegistry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(files) = self.files.get() {
            Debug::fmt(files, f)
        } else {
            f.debug_list().finish()
        }
    }
}

/// Identifier for a package, guaranteed to be valid.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId(usize);
impl PackageId {
    /// The root package, should be used as default.
    pub const ROOT: PackageId = PackageId(0);
    /// Get the name associated with the package.
    pub fn name(self) -> &'static str {
        &FILE_REGISTRY.get()[self.0].0
    }
}
impl Debug for PackageId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = self.name();
        f.debug_tuple("PackageId")
            .field(if self.0 == 0 {
                &ROOT as &dyn Debug
            } else {
                &name as _
            })
            .finish()
    }
}
impl Display for PackageId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A unique identifier for a file, also acts as source code for miette.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(usize, usize);
impl FileId {
    pub fn package(self) -> PackageId {
        PackageId(self.0)
    }
    /// Get the name of the containing package.
    pub fn pkg_name(self) -> &'static str {
        &FILE_REGISTRY.get()[self.0].0
    }
    /// Get the name of the file. Note that this doesn't include the package.
    pub fn file(self) -> &'static str {
        FILE_REGISTRY.get()[self.0].1[self.1].name()
    }
    /// Get the contents of the file
    pub fn contents(self) -> &'static [u8] {
        FILE_REGISTRY.get()[self.0].1[self.1].inner()
    }
}
impl Debug for FileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (pkg, files) = &FILE_REGISTRY.get()[self.0];
        f.debug_tuple("FileId")
            .field(if self.0 == 0 {
                &ROOT as &dyn Debug
            } else {
                &pkg as _
            })
            .field(&files[self.1].name())
            .finish()
    }
}
/// Display as "package//path" for non-root packages
impl Display for FileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (pkg, files) = &FILE_REGISTRY.get()[self.0];
        if self.0 != 0 {
            f.write_str(pkg)?;
            f.write_str("//")?;
        }
        f.write_str(files[self.1].name())
    }
}
impl SourceCode for FileId {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        self.contents()
            .read_span(span, context_lines_before, context_lines_after)
    }
}

pub static FILE_REGISTRY: FileRegistry = FileRegistry::new();
