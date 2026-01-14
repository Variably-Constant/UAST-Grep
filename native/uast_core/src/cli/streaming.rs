//! Streaming support for scan command
//!
//! This module provides channel-based streaming for scan results to avoid
//! accumulating all results in memory, which can cause OOM on large codebases.

use crossbeam_channel::{bounded, Receiver, Sender};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::rules::{ScanResult, Severity};

/// Message sent through the streaming channel
pub enum ScanMessage {
    /// A scan result to be output
    Result(ScanResult),
}

/// Statistics tracked during streaming scan
///
/// Uses atomic counters for thread-safe updates from parallel workers.
pub struct StreamingStats {
    pub files_processed: AtomicU64,
    pub total_results: AtomicU64,
    pub error_count: AtomicU64,
    pub warning_count: AtomicU64,
    pub info_count: AtomicU64,
    pub hint_count: AtomicU64,
}

impl StreamingStats {
    /// Create new statistics tracker with all counters at zero
    pub fn new() -> Self {
        Self {
            files_processed: AtomicU64::new(0),
            total_results: AtomicU64::new(0),
            error_count: AtomicU64::new(0),
            warning_count: AtomicU64::new(0),
            info_count: AtomicU64::new(0),
            hint_count: AtomicU64::new(0),
        }
    }

    /// Increment the counter for the given severity
    pub fn increment_severity(&self, severity: Severity) {
        self.total_results.fetch_add(1, Ordering::Relaxed);
        match severity {
            Severity::Error => self.error_count.fetch_add(1, Ordering::Relaxed),
            Severity::Warning => self.warning_count.fetch_add(1, Ordering::Relaxed),
            Severity::Info => self.info_count.fetch_add(1, Ordering::Relaxed),
            Severity::Hint => self.hint_count.fetch_add(1, Ordering::Relaxed),
        };
    }

    /// Increment files processed counter
    pub fn increment_files(&self) {
        self.files_processed.fetch_add(1, Ordering::Relaxed);
    }

    /// Get total results count
    pub fn get_total_results(&self) -> u64 {
        self.total_results.load(Ordering::Relaxed)
    }

    /// Get error count
    pub fn get_error_count(&self) -> u64 {
        self.error_count.load(Ordering::Relaxed)
    }

    /// Get files processed count
    pub fn get_files_processed(&self) -> u64 {
        self.files_processed.load(Ordering::Relaxed)
    }
}

impl Default for StreamingStats {
    fn default() -> Self {
        Self::new()
    }
}

/// Create a bounded channel for streaming scan results
///
/// The buffer_size controls backpressure - when the channel is full,
/// producers will block until the consumer catches up.
pub fn create_channel(buffer_size: usize) -> (Sender<ScanMessage>, Receiver<ScanMessage>) {
    bounded(buffer_size)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_streaming_stats_new() {
        let stats = StreamingStats::new();
        assert_eq!(stats.get_total_results(), 0);
        assert_eq!(stats.get_error_count(), 0);
        assert_eq!(stats.get_files_processed(), 0);
    }

    #[test]
    fn test_streaming_stats_increment() {
        let stats = StreamingStats::new();

        stats.increment_severity(Severity::Error);
        stats.increment_severity(Severity::Warning);
        stats.increment_severity(Severity::Error);

        assert_eq!(stats.get_total_results(), 3);
        assert_eq!(stats.get_error_count(), 2);
        assert_eq!(stats.warning_count.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn test_create_channel() {
        let (tx, rx) = create_channel(10);

        // Channel should be empty initially
        assert!(rx.try_recv().is_err());

        // Should be able to send
        assert!(tx.send(ScanMessage::Result(ScanResult::new(
            "test".to_string(),
            Severity::Info,
            "test message".to_string(),
            crate::uast::SourceSpan::default(),
        ))).is_ok());

        // Should be able to receive
        assert!(rx.try_recv().is_ok());
    }
}
