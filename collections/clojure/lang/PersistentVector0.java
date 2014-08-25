package clojure.lang;

import java.util.Iterator;

public class PersistentVector0 extends APersistentVector implements IObj,
	IEditableCollection, IReduce {
    private final IPersistentMap meta;
    private int hash = -1;
    private int hasheq = -1;
    public final static PersistentVector0 EMPTY = new PersistentVector0();

    PersistentVector0(IPersistentMap meta) {
	this.meta = meta;
    }

    public PersistentVector0() {
	this.meta = null;
    }

    public IPersistentMap meta() {
	return meta;
    }

    public IObj withMeta(IPersistentMap meta) {
	return new PersistentVector0(meta);
    }

    public Object nth(int i) {
	throw new IndexOutOfBoundsException();
    }

    public Object nth(int i, Object notFound) {
	return notFound;
    }

    public int count() {
	return 0;
    }

    public IPersistentVector empty() {
	return PersistentVector0.EMPTY;
    }

    public IPersistentVector assocN(int i, Object val) {
	switch (i) {
	case 0:
	    return cons(val);
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public IPersistentVector cons(Object val) {
	return new PersistentVector1(meta, val);
    }

    public ITransientCollection asTransient() {
	ITransientCollection coll = PersistentVector.EMPTY.asTransient();
	return (ITransientVector) coll;
    }

    public IPersistentVector pop() {
	throw new IllegalStateException("Can't pop empty vector");
    }

    public Object kvreduce(IFn f, Object init) {
	return init;
    }

    public Object reduce(IFn f) {
	return f.invoke();
    }

    public Object reduce(IFn f, Object init) {
	return init;
    }

    public int hashCode() {
	if (this.hash == -1) {
	    int hash = 1;
	    this.hash = hash;
	}
	return hash;
    }

    public int hasheq() {
	if (this.hasheq == -1) {
	    int hash = 1;
	    hash = Murmur3.mixCollHash(hash, 0);
	    this.hasheq = hash;
	}
	return hasheq;
    }

    public boolean equals(Object o) {
	return o == this ? true : super.equals(o);
    }

    public boolean equiv(Object o) {
	return o == this ? true : super.equiv(o);
    }

    public Iterator iterator() {
	return new Iterator() {
	    int i = 0;

	    public boolean hasNext() {
		return i < 0;
	    }

	    public Object next() {
		return nth(i++);
	    }

	    public void remove() {
		throw new UnsupportedOperationException();
	    }
	};
    }

    public ISeq seq() {
	return IteratorSeq.create(iterator());
    }
}