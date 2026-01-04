//! Common types for Python bindings.

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;

/// Convert a Rust error to a Python exception.
pub fn to_py_err<E: std::fmt::Display>(err: E) -> PyErr {
    PyValueError::new_err(err.to_string())
}

/// Python-compatible severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PySeverity {
    Error,
    Warning,
    Info,
    Hint,
}

impl From<crate::rules::Severity> for PySeverity {
    fn from(s: crate::rules::Severity) -> Self {
        match s {
            crate::rules::Severity::Error => PySeverity::Error,
            crate::rules::Severity::Warning => PySeverity::Warning,
            crate::rules::Severity::Info => PySeverity::Info,
            crate::rules::Severity::Hint => PySeverity::Hint,
        }
    }
}

impl IntoPy<PyObject> for PySeverity {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self {
            PySeverity::Error => "error".into_py(py),
            PySeverity::Warning => "warning".into_py(py),
            PySeverity::Info => "info".into_py(py),
            PySeverity::Hint => "hint".into_py(py),
        }
    }
}
