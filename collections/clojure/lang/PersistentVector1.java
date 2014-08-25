package clojure.lang;

import java.util.Iterator;

public class PersistentVector1 extends APersistentVector implements IObj,
	IEditableCollection, IReduce {
    final Object e0;
    private final IPersistentMap meta;
    private int hash = -1;
    private int hasheq = -1;

    PersistentVector1(IPersistentMap meta, Object e0) {
	this.meta = meta;
	this.e0 = e0;
    }

    public PersistentVector1(Object e0) {
	this.meta = null;
	this.e0 = e0;
    }

    public IPersistentMap meta() {
	return meta;
    }

    public IObj withMeta(IPersistentMap meta) {
	return new PersistentVector1(meta, e0);
    }

    public Object nth(int i) {
	switch (i) {
	case 0:
	    return e0;
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public Object nth(int i, Object notFound) {
	switch (i) {
	case 0:
	    return e0;
	default:
	    return notFound;
	}
    }

    public int count() {
	return 1;
    }

    public IPersistentVector empty() {
	return PersistentVector0.EMPTY;
    }

    public IPersistentVector assocN(int i, Object val) {
	switch (i) {
	case 0:
	    return new PersistentVector1(meta, val);
	case 1:
	    return cons(val);
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public IPersistentVector cons(Object val) {
	return new PersistentVector2(meta, e0, val);
    }

    public ITransientCollection asTransient() {
	ITransientCollection coll = PersistentVector.EMPTY.asTransient();
	return (ITransientVector) coll.conj(e0);
    }

    public IPersistentVector pop() {
	return new PersistentVector0(meta);
    }

    public Object kvreduce(IFn f, Object init) {
	init = f.invoke(init, 0, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f) {
	return e0;
    }

    public Object reduce(IFn f, Object init) {
	init = f.invoke(init, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public int hashCode() {
	if (this.hash == -1) {
	    int hash = 1;
	    hash = (31 * hash) + (e0 == null ? 0 : e0.hashCode());
	    this.hash = hash;
	}
	return hash;
    }

    public int hasheq() {
	if (this.hasheq == -1) {
	    int hash = 1;
	    hash = (31 * hash) + Util.hasheq(e0);
	    hash = Murmur3.mixCollHash(hash, 1);
	    this.hasheq = hash;
	}
	return hasheq;
    }

    public boolean equals(Object o) {
	if (o instanceof PersistentVector1) {
	    return Util.equals(e0, ((PersistentVector1) o).e0);
	} else {
	    return super.equals(o);
	}
    }

    public boolean equiv(Object o) {
	if (o instanceof PersistentVector1) {
	    return Util.equiv(e0, ((PersistentVector1) o).e0);
	} else {
	    return super.equiv(o);
	}
    }

    public Iterator iterator() {
	return new Iterator() {
	    int i = 0;

	    public boolean hasNext() {
		return i < 1;
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