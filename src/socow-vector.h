#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <memory>
#include <new>
#include <utility>

template <typename T, size_t SMALL_SIZE>
class socow_vector {
  struct dynamic_buffer {
    size_t capacity_;
    size_t refs_count;
    T data_[0];

    explicit dynamic_buffer(size_t capacity) : capacity_(capacity), refs_count(1) {}

    T* data() {
      return data_;
    }

    size_t capacity() {
      return capacity_;
    }
  };

  std::size_t size_;
  bool is_small_vector_;

  union {
    dynamic_buffer* dynamic_data;
    std::array<T, SMALL_SIZE> static_buffer;
  };

public:
  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

  using iterator = pointer;
  using const_iterator = const_pointer;

public:
  socow_vector() noexcept : size_(0), is_small_vector_(true), dynamic_data(nullptr) {}

  socow_vector(const socow_vector& other) : size_(other.size()), is_small_vector_(other.is_small_vector_) {
    if (is_small_vector_) {
      std::uninitialized_copy(other.const_data(), other.const_data() + other.size(), data());
    } else {
      dynamic_data = other.dynamic_data;
      add_vector_ref();
    }
  }

  socow_vector& operator=(const socow_vector& other) {
    if (is_same_vector(other)) {
      return *this;
    }
    if (is_small_vector_ && other.is_small_vector_) {
      socow_vector tmp;
      std::uninitialized_copy(other.begin(), other.begin() + std::min(size(), other.size()), tmp.begin());
      tmp.size_ = std::min(size(), other.size());
      std::uninitialized_copy(other.begin() + tmp.size(), other.end(), begin() + tmp.size());
      while (size() > other.size()) {
        pop_back();
      }
      size_ = other.size();
      for (size_t i = 0; i < tmp.size(); ++i) {
        std::swap(tmp.static_buffer[i], static_buffer[i]);
      }
    } else if (other.is_small_vector_) {
      from_dynamic_to_static(other.const_data(), other.size());
    } else {
      if (is_small_vector_) {
        noexcept_clear();
        is_small_vector_ = false;
      } else {
        release_vector_ref();
      }
      dynamic_data = other.dynamic_data;
      add_vector_ref();
      size_ = other.size();
    }
    return *this;
  }

  ~socow_vector() noexcept {
    if (is_small_vector_) {
      noexcept_clear();
    } else {
      release_vector_ref();
    }
  }

  reference operator[](size_t index) {
    return data()[index];
  }

  const_reference operator[](size_t index) const {
    return const_data()[index];
  }

  size_t size() const noexcept {
    return size_;
  }

  pointer data() {
    unshare();
    return is_small_vector_ ? static_buffer.data() : dynamic_data->data();
  }

  const_pointer data() const noexcept {
    return is_small_vector_ ? static_buffer.data() : dynamic_data->data();
  }

  reference front() {
    return *begin();
  }

  const_reference front() const {
    return *begin();
  }

  reference back() {
    return *(end() - 1);
  }

  const_reference back() const {
    return *(end() - 1);
  }

  void push_back(const T& value) {
    insert(const_data() + size(), value);
  }

  void pop_back() {
    if (is_only_vector_ref()) {
      const_data()[--size_].~T();
    } else {
      socow_vector tmp(*this, size() - 1, capacity());
      swap(tmp);
    }
  }

  bool empty() const noexcept {
    return size() == 0;
  }

  size_t capacity() const noexcept {
    return is_small_vector_ ? SMALL_SIZE : dynamic_data->capacity();
  }

  void reserve(size_t new_capacity) {
    if (new_capacity < size()) {
      return;
    }
    if (!is_only_vector_ref() && new_capacity <= SMALL_SIZE) {
      from_dynamic_to_static(const_data(), size());
      return;
    }

    if (new_capacity > capacity() || (!is_only_vector_ref() && new_capacity >= size())) {
      if (is_small_vector_) {
        socow_vector tmp(*this, size(), new_capacity);
        *this = tmp;
      } else {
        socow_vector(*this, size(), new_capacity).swap(*this);
      }
    }
  }

  void shrink_to_fit() {
    if (is_small_vector_ || size() == capacity()) {
      return;
    }
    if (size() <= SMALL_SIZE) {
      from_dynamic_to_static(const_data(), size());
      return;
    }
    socow_vector(*this, size(), size()).swap(*this);
  }

  void clear() {
    if (!is_only_vector_ref()) {
      socow_vector tmp;
      tmp.reserve(size());
      swap(tmp);
      return;
    }
    std::destroy_n(const_data(), size());
    size_ = 0;

    bool was_only_ref = is_only_vector_ref();
    noexcept_clear();
    if (!was_only_ref) {
      allocate_dynamic_buffer(capacity());
      is_small_vector_ = false;
    }
  }

  void swap(socow_vector& other) {
    if (is_same_vector(other)) {
      return;
    }
    if (is_small_vector_ && other.is_small_vector_) {
      if (size() > other.size()) {
        other.swap(*this);
        return;
      }
      std::uninitialized_copy(other.begin() + size(), other.end(), end());
      size_t tmp_size = size();
      size_ = other.size();
      while (other.size() > tmp_size) {
        other.pop_back();
      }
      for (size_t i = 0; i < tmp_size; ++i) {
        std::swap(other.static_buffer[i], static_buffer[i]);
      }
    } else if (other.is_small_vector_) {
      dynamic_buffer* tmp = dynamic_data;
      size_t tmp_size = size();
      is_small_vector_ = true;
      dynamic_data = nullptr;
      try {
        std::uninitialized_copy(other.const_data(), other.const_data() + other.size(), begin());
      } catch (...) {
        is_small_vector_ = false;
        dynamic_data = tmp;
        throw;
      }
      size_ = other.size();
      other.noexcept_clear();
      other.is_small_vector_ = false;
      other.dynamic_data = tmp;
      other.size_ = tmp_size;
    } else if (is_small_vector_) {
      other.swap(*this);
    } else {
      std::swap(dynamic_data, other.dynamic_data);
      std::swap(size_, other.size_);
    }
  }

  iterator begin() {
    return data();
  }

  iterator end() {
    return begin() + size();
  }

  const_iterator begin() const noexcept {
    return const_data();
  }

  const_iterator end() const noexcept {
    return begin() + size();
  }

  iterator insert(const_iterator pos, const T& value) {
    ptrdiff_t delta = pos - const_data();
    if (size() != capacity() && is_only_vector_ref()) {
      new (data() + size()) T(value);
      ++size_;

      iterator spot = begin() + delta;
      for (auto i = end() - 1; i > spot; --i) {
        std::iter_swap(i, i - 1);
      }
      return spot;
    } else {
      socow_vector tmp;
      tmp.allocate_dynamic_buffer(size() == capacity() ? 2 * capacity() : capacity());
      tmp.is_small_vector_ = false;

      std::uninitialized_copy(const_data(), const_data() + delta, tmp.dynamic_data->data());
      tmp.size_ = delta;
      new (tmp.dynamic_data->data() + delta) T(value);
      ++tmp.size_;
      std::uninitialized_copy(const_data() + delta, const_data() + size(), tmp.dynamic_data->data() + delta + 1);
      tmp.size_ = size() + 1;
      noexcept_clear();
      swap(tmp);
      return begin() + delta;
    }
  }

  iterator erase(const_iterator pos) {
    return erase(pos, pos + 1);
  }

  iterator erase(const_iterator first, const_iterator last) {
    ptrdiff_t interval = last - first;
    ptrdiff_t delta = first - std::as_const(*this).begin();
    if (is_only_vector_ref()) {
      if (interval != 0) {
        for (auto i = const_cast<iterator>(first); i + interval < end(); ++i) {
          std::iter_swap(i, i + interval);
        }
      }
      for (auto i = 0; i < interval; i++) {
        pop_back();
      }
    } else {
      socow_vector tmp(*this, delta, capacity());
      std::uninitialized_copy(const_data() + delta + interval, const_data() + size(), tmp.begin() + delta);
      tmp.size_ = size() - interval;
      swap(tmp);
    }
    return begin() + delta;
  }

private:
  void noexcept_clear() noexcept {
    if (!is_only_vector_ref()) {
      is_small_vector_ = true;
      size_ = 0;
      --dynamic_data->refs_count;
      return;
    }
    std::destroy_n(const_data(), size());
    size_ = 0;
  }

  bool is_same_vector(const socow_vector& other) const {
    return const_data() == other.const_data();
  }

  void from_dynamic_to_static(const_pointer data_pointer, size_t count) {
    assert(count <= SMALL_SIZE);
    dynamic_buffer* tmp = dynamic_data;
    is_small_vector_ = true;
    dynamic_data = nullptr;
    try {
      std::uninitialized_copy(data_pointer, data_pointer + count, begin());
      release_ref(tmp, size());
      size_ = count;
    } catch (...) {
      is_small_vector_ = false;
      dynamic_data = tmp;
      throw;
    }
  }

  void allocate_dynamic_buffer(size_t capacity) {
    dynamic_data = new (static_cast<dynamic_buffer*>(operator new(sizeof(dynamic_buffer) + capacity * sizeof(T))))
        dynamic_buffer(capacity);
  }

  bool is_only_vector_ref() {
    return is_small_vector_ || dynamic_data->refs_count == 1;
  }

  const_pointer const_data() const noexcept {
    return std::as_const(*this).data();
  }

  void add_vector_ref() {
    if (is_small_vector_) {
      return;
    }
    ++dynamic_data->refs_count;
  }

  void release_vector_ref() {
    if (is_small_vector_) {
      return;
    }
    release_ref(dynamic_data, size());
  }

  static void release_ref(dynamic_buffer* dynamic_data, size_t count) {
    if (--dynamic_data->refs_count == 0) {
      std::destroy_n(dynamic_data->data(), count);
      operator delete(dynamic_data);
    }
  }

  void unshare() {
    if (is_only_vector_ref()) {
      return;
    }
    socow_vector tmp(*this, size(), capacity());
    swap(tmp);
  }

  socow_vector(const socow_vector& other, size_t new_size, size_t new_capacity)
      : size_(new_size),
        is_small_vector_(new_capacity <= SMALL_SIZE) {
    if (!is_small_vector_) {
      allocate_dynamic_buffer(new_capacity);
    }
    try {
      std::uninitialized_copy(other.const_data(), other.const_data() + new_size, data());
    } catch (...) {
      if (!is_small_vector_) {
        operator delete(dynamic_data);
      }
      throw;
    }
  }
};
